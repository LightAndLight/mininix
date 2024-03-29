module Builder.Parallel (buildInParallel, BuildLogger (..), ParBuildError (..)) where

import Action (Action (..))
import Builder (BuildError)
import Builder.Logging (BuildLogger (..))
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar, stateTVar, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad.Loops (whileJust_)
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Key (Key)
import Store (Store (..))
import Prelude hiding (error, log)

data ParBuildError
  = Exception {threadId :: Int, value :: SomeException}
  | BuildError {threadId :: Int, buildError :: BuildError}
  deriving (Show)

data ParBuildState = ParBuildState
  { threadsRunning :: TVar Int
  , tasks :: TQueue Key
  , output :: TQueue String
  , graph :: TVar [(Action, Key, [Key])]
  , error :: TVar (Maybe ParBuildError)
  }

partitionQueueable :: [(action, key, [dependency])] -> ([key], [(action, key, [dependency])])
partitionQueueable =
  Either.partitionEithers
    . fmap
      ( \entry@(_, key, deps) ->
          if null deps then Left key else Right entry
      )

deleteMatchingDependencies ::
  (dependency -> Bool) ->
  (action, key, [dependency]) ->
  (action, key, [dependency])
deleteMatchingDependencies predicate (action, key', dependencies) =
  ( action
  , key'
  , List.filter (not . predicate) dependencies
  )

getNextOutput :: ParBuildState -> STM (Maybe String)
getNextOutput parBuildState =
  Just <$> readTQueue parBuildState.output <|> do
    jobsRunning <- readTVar parBuildState.threadsRunning
    if jobsRunning == 0 then pure Nothing else retry

buildInParallel :: (BuildLogger -> Key -> IO (Either BuildError ())) -> Store -> [(Action, Key, [Key])] -> Key -> IO (Either ParBuildError Key)
buildInParallel buildKey store input key = do
  threads <- getNumCapabilities
  putStrLn $ "using " <> show threads <> " thread" <> (if threads == 1 then "" else "s")

  let jobs = max 1 (threads - 1)

  tasks :: TQueue Key <- newTQueueIO
  output :: TQueue String <- newTQueueIO
  graph :: TVar [(Action, Key, [Key])] <- newTVarIO input
  threadsRunning :: TVar Int <- newTVarIO jobs
  error :: TVar (Maybe ParBuildError) <- newTVarIO Nothing

  atomically $ do
    queueable <- stateTVar graph partitionQueueable
    traverse_ (writeTQueue tasks) queueable

  let parBuildState = ParBuildState{threadsRunning, tasks, output, graph, error}
  let exit = atomically $ modifyTVar parBuildState.threadsRunning (subtract 1)
  let continue = id

  traverse_
    (forkIO . waitForTask exit continue parBuildState)
    [0 .. jobs - 1]

  whileJust_ (atomically $ getNextOutput parBuildState) putStr
  mError <- atomically $ readTVar error
  case mError of
    Nothing -> maybe undefined (pure . Right) =<< store.getMemo key
    Just err -> pure $ Left err
 where
  isEmpty :: TVar [(Action, Key, [Key])] -> STM Bool
  isEmpty = fmap null . readTVar

  checkIfError :: ParBuildState -> STM Bool
  checkIfError parBuildState =
    Maybe.isJust <$> readTVar parBuildState.error

  getNextTask :: ParBuildState -> STM (Maybe Key)
  getNextTask parBuildState =
    ( do
        hasError <- checkIfError parBuildState
        if hasError then pure Nothing else retry
    )
      <|> Just <$> readTQueue parBuildState.tasks
      <|> do
        empty <- isEmpty parBuildState.graph
        if empty then pure Nothing else retry

  waitForTask :: IO a -> (IO a -> IO a) -> ParBuildState -> Int -> IO a
  waitForTask exit continue parBuildState threadId = do
    mNextTask <- atomically $ getNextTask parBuildState
    case mNextTask of
      Nothing -> do
        exit
      Just nextTask ->
        continue $ processTask exit continue parBuildState nextTask threadId

  processTask :: IO a -> (IO a -> IO a) -> ParBuildState -> Key -> Int -> IO a
  processTask exit continue parBuildState taskKey threadId = do
    hasError <- atomically $ checkIfError parBuildState
    if hasError
      then exit
      else do
        let log str =
              atomically $
                writeTQueue
                  parBuildState.output
                  ("thread " <> show threadId <> ": " <> str)

        exceptionOrResult <- try $ buildKey BuildLogger{log, logLine = log . (++ "\n")} taskKey
        case exceptionOrResult of
          Left err -> do
            atomically . writeTVar parBuildState.error $ Just Exception{threadId, value = err}
            exit
          Right result ->
            case result of
              Left buildError -> do
                atomically . writeTVar parBuildState.error $ Just BuildError{threadId, buildError}
                exit
              Right () -> do
                queueable <-
                  atomically $
                    stateTVar
                      parBuildState.graph
                      (partitionQueueable . fmap (deleteMatchingDependencies (== taskKey)))

                case queueable of
                  [] ->
                    continue $ waitForTask exit continue parBuildState threadId
                  nextTask : rest -> do
                    atomically $ traverse_ (writeTQueue parBuildState.tasks) rest
                    continue $ processTask exit continue parBuildState nextTask threadId
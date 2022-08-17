module Builder.Parallel (buildInParallel, BuildLogger (..)) where

import Action (Action (..))
import Builder.Logging (BuildLogger (..))
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar, stateTVar)
import Control.Monad.Loops (whileJust_)
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import qualified Data.List as List
import Key (Key)
import Prelude hiding (log)

data ParBuildState = ParBuildState
  { threadsRunning :: TVar Int
  , tasks :: TQueue Key
  , output :: TQueue String
  , graph :: TVar [(Action, Key, [Key])]
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

buildInParallel :: (BuildLogger -> Key -> IO ()) -> [(Action, Key, [Key])] -> IO ()
buildInParallel buildKey input = do
  threads <- getNumCapabilities
  putStrLn $ "using " <> show threads <> " thread" <> (if threads == 1 then "" else "s")

  let jobs = max 1 (threads - 1)

  tasks :: TQueue Key <- newTQueueIO
  output :: TQueue String <- newTQueueIO
  graph :: TVar [(Action, Key, [Key])] <- newTVarIO input
  threadsRunning :: TVar Int <- newTVarIO jobs

  atomically $ do
    queueable <- stateTVar graph partitionQueueable
    traverse_ (writeTQueue tasks) queueable

  let parBuildState = ParBuildState{threadsRunning, tasks, output, graph}
  traverse_
    (forkIO . waitForTask parBuildState)
    [0 .. jobs - 1]

  whileJust_ (atomically $ getNextOutput parBuildState) putStr
 where
  isEmpty :: TVar [(Action, Key, [Key])] -> STM Bool
  isEmpty = fmap null . readTVar

  getNextTask :: ParBuildState -> STM (Maybe Key)
  getNextTask parBuildState =
    Just <$> readTQueue parBuildState.tasks <|> do
      empty <- isEmpty parBuildState.graph
      if empty then pure Nothing else retry

  waitForTask :: ParBuildState -> Int -> IO ()
  waitForTask parBuildState threadId = do
    mNextTask <- atomically $ getNextTask parBuildState
    case mNextTask of
      Nothing -> do
        atomically $ modifyTVar parBuildState.threadsRunning (subtract 1)
      Just nextTask ->
        processTask parBuildState nextTask threadId

  processTask :: ParBuildState -> Key -> Int -> IO ()
  processTask parBuildState key threadId = do
    let log str =
          atomically $
            writeTQueue
              parBuildState.output
              ("thread " <> show threadId <> ": " <> str)

    buildKey BuildLogger{log, logLine = log . (++ "\n")} key

    queueable <-
      atomically $
        stateTVar
          parBuildState.graph
          (partitionQueueable . fmap (deleteMatchingDependencies (== key)))

    case queueable of
      [] ->
        waitForTask parBuildState threadId
      nextTask : rest -> do
        atomically $ traverse_ (writeTQueue parBuildState.tasks) rest
        processTask parBuildState nextTask threadId
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Action (Action (..))
import Control.Applicative ((<|>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, stateTVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Eval
import qualified Expr
import Key (Key)
import Store (Store)
import qualified Store
import qualified System.Directory as Directory
import System.Exit (ExitCode (..))
import qualified System.IO
import qualified System.Process as Process
import Value (Value)
import qualified Value
import Prelude hiding (log)

buildInParallel :: MonadIO m => Store -> [(Action, Key, [Key])] -> m ()
buildInParallel store graph = do
  queue :: TQueue Key <- liftIO newTQueueIO
  let (graph', queueable) =
        Either.partitionEithers $
          fmap
            ( \entry@(_, key, deps) ->
                if null deps then Right key else Left entry
            )
            graph
  liftIO . atomically $ traverse_ (writeTQueue queue) queueable

  logs :: TQueue String <- liftIO newTQueueIO

  nodes :: TVar [(Action, Key, [Key])] <- liftIO $ newTVarIO graph'
  threads <- liftIO getNumCapabilities

  let jobs = max 1 (threads - 1)

  liftIO . putStrLn $ "using " <> show threads <> " thread" <> (if threads == 1 then "" else "s")

  _ <- traverse (liftIO . async . waitForTask queue logs nodes) [0 .. jobs - 1]

  let loop = do
        mLine <-
          atomically $
            Just <$> readTQueue logs <|> do
              jobsRunning <- readTVar jobsRunningVar
              if jobsRunning == 0 then pure Nothing else retry
        case mLine of
          Nothing ->
            pure ()
          Just line ->
            putStrLn line

  liftIO loop
  where
    isFinished :: TVar [(Action, Key, [Key])] -> STM Bool
    isFinished = fmap null . readTVar

    processTask :: TQueue Key -> TQueue String -> TVar [(Action, Key, [Key])] -> Key -> Int -> IO ()
    processTask queue logs nodesVar key threadId = do
      _ <-
        runWithoutBuildingDeps
          (\str -> atomically $ writeTQueue logs $ "thread " <> show threadId <> ": " <> str)
          store
          key

      queueable <-
        atomically $
          stateTVar
            nodesVar
            ( \nodes ->
                Either.partitionEithers $
                  ( \(nodeAction, nodeKey, nodeDeps) ->
                      case List.delete key nodeDeps of
                        [] -> Left nodeKey
                        nodeDeps'@(_ : _) -> Right (nodeAction, nodeKey, nodeDeps')
                  )
                    <$> nodes
            )

      case queueable of
        [] -> waitForTask queue logs nodesVar threadId
        nextTask : rest -> do
          atomically $ traverse_ (writeTQueue queue) rest
          processTask queue logs nodesVar nextTask threadId

    waitForTask :: TQueue Key -> TQueue String -> TVar [(Action, Key, [Key])] -> Int -> IO ()
    waitForTask queue logs nodes threadId = do
      mNextTask <-
        atomically $
          Just <$> readTQueue queue <|> do
            finished <- isFinished nodes
            if finished then pure Nothing else retry
      case mNextTask of
        Nothing ->
          pure ()
        Just nextTask ->
          processTask queue logs nodes nextTask threadId

runWithoutBuildingDeps :: MonadIO m => (String -> IO ()) -> Store -> Key -> m Value
runWithoutBuildingDeps log store key = do
  do
    let path = store.keyPath key
    liftIO . log $ "running action " <> path

  mOutputKey <- liftIO $ store.getMemo key
  case mOutputKey of
    Nothing -> do
      liftIO $ log "  no cached result found, building"

      object <- liftIO $ Maybe.fromMaybe undefined <$> store.get key
      case object of
        Store.Action (Action inputs builderKey) -> do
          inputs' :: [(Text, FilePath)] <-
            traverse
              ( \(inputName, input) -> do
                  let inputPath = store.keyPath input
                  inputResultKey <-
                    liftIO $
                      maybe (error $ "no result found for " <> inputPath) pure
                        =<< store.getMemo input
                  pure (inputName, store.keyPath inputResultKey)
              )
              inputs

          let builderExecutable = store.keyPath builderKey

          liftIO $ do
            p <- Directory.getPermissions builderExecutable
            Directory.setPermissions builderExecutable p {Directory.executable = True}

          liftIO . log $
            "running builder: "
              <> foldMap (\(name, input) -> Text.unpack name <> "=" <> input <> " ") inputs'
              <> builderExecutable

          (_mhStdin, mhStdout, mhStderr, processHandle) <-
            liftIO $
              Process.createProcess
                ( ( Process.proc
                      builderExecutable
                      []
                  )
                    { Process.std_in = Process.Inherit,
                      Process.std_out = Process.CreatePipe,
                      Process.std_err = Process.CreatePipe,
                      Process.env = Just $ first Text.unpack <$> inputs'
                    }
                )

          case (mhStdout, mhStderr) of
            (Just hStdout, Just hStderr) -> do
              contents <- liftIO $ do
                stdout <- ByteString.hGetContents hStdout
                stderr <- System.IO.hGetContents hStderr

                exitCode <- Process.waitForProcess processHandle

                unless (exitCode == ExitSuccess) . error $ builderExecutable <> " failed"

                unless (null stderr) $ log stderr

                pure stdout

              outputKey <- liftIO $ store.put (Store.Object contents)
              liftIO $ store.putMemo key outputKey
              pure $ Value.Object outputKey
            _ ->
              undefined
        _ ->
          error $ "expected action, got: " <> show object
    Just outputKey -> do
      let outputPath = store.keyPath outputKey
      liftIO . log $ "  cached result found (" <> outputPath <> "), skipping build"
      pure $ Value.Object outputKey

getDependencyGraph :: forall m. MonadIO m => Store -> Key -> m [(Action, Key, [Key])]
getDependencyGraph store key =
  Map.foldrWithKey (\k (a, dependencies) rest -> (a, k, dependencies) : rest) []
    <$> go key
  where
    go :: Key -> m (Map Key (Action, [Key]))
    go k = do
      mObject <- liftIO $ store.get k
      case mObject of
        Just (Store.Action a@(Action inputs _)) ->
          Map.insert k (a, snd <$> inputs) . mconcat <$> traverse (go . snd) inputs
        _ ->
          pure mempty

run :: MonadIO m => Store -> Key -> m ()
run store key = do
  do
    let path = store.keyPath key
    liftIO . putStrLn $ "running action " <> path

  mOutputKey <- liftIO $ store.getMemo key
  case mOutputKey of
    Nothing -> do
      liftIO $ putStrLn "  no cached result found, building"

      dependencyGraph <- getDependencyGraph store key
      buildInParallel store dependencyGraph
    Just outputKey -> do
      let outputPath = store.keyPath outputKey
      liftIO . putStrLn $ "  cached result found (" <> outputPath <> "), skipping build"

main :: IO ()
main = do
  store <- Store.new "./store" "database.json"
  action <-
    Eval.eval store []
      $
      {-
        action {
          inputs = {}
          builder = file "./builder.sh"
        }
      -}
      Expr.Let
        "hello"
        ( Expr.Action
            ( Expr.Record
                [ ("inputs", Expr.Record []),
                  ("builder", Expr.File "./hello.sh")
                ]
            )
        )
        . Expr.Let
          "goodbye"
          ( Expr.Action
              ( Expr.Record
                  [ ("inputs", Expr.Record []),
                    ("builder", Expr.File "./goodbye.sh")
                  ]
              )
          )
      $ Expr.Action
        ( Expr.Record
            [ ( "inputs",
                Expr.Record
                  [ ("hello", Expr.Var "hello"),
                    ("goodbye", Expr.Var "goodbye")
                  ]
              ),
              ("builder", Expr.File "./concat.sh")
            ]
        )

  case action of
    Value.Action key -> do
      run store key
      outputKey <- maybe undefined pure =<< store.getMemo key
      putStrLn $ "build " <> store.keyPath outputKey
    _ -> undefined
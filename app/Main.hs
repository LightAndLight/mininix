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
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import qualified Data.Graph as Graph
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Eval
import qualified Expr
import FileStore (FileStoreEnv (..), runFileStoreT)
import Key (Key)
import Object (Object)
import qualified Object
import Store (MonadStore, get, getMemo, keyPath, put, putMemo)
import qualified System.Directory as Directory
import System.Exit (ExitCode (..))
import qualified System.Process as Process
import Value (Value)
import qualified Value

getDependencyGraph :: MonadStore Key Object m => Action -> m [(Action, Key, [Key])]
getDependencyGraph =
  fmap
    (Map.foldrWithKey (\key (action, dependencies) rest -> (action, key, dependencies) : rest) [])
    . go
  where
    go :: MonadStore Key Object m => Action -> m (Map Key (Action, [Key]))
    go (Action inputs _) =
      mconcat
        <$> traverse
          ( \(_, input) -> do
              mObject <- get input
              case mObject of
                Just (Object.Action action) ->
                  Map.insert input (action, snd <$> action.inputs) <$> go action
                _ ->
                  pure mempty
          )
          inputs

buildInParallel :: (MonadStore Key Object m, MonadIO m) => [(Action, Key, [Key])] -> m ()
buildInParallel graph = do
  queue :: TQueue Key <- liftIO newTQueueIO
  let (graph', queueable) =
        Either.partitionEithers $
          fmap
            ( \entry@(_, key, deps) ->
                if null deps then Right key else Left entry
            )
            graph
  liftIO . atomically $ traverse_ (writeTQueue queue) queueable

  nodes :: TVar [(Action, Key, [Key])] <- liftIO $ newTVarIO graph'
  threads <- liftIO getNumCapabilities

  let backgroundThreads = max 1 (threads - 1)

  asyncs <- traverse (liftIO . async . waitForTask queue nodes) [0 .. backgroundThreads - 1]

  _ asyncs
  where
    isFinished :: TVar [(Action, Key, [Key])] -> STM Bool
    isFinished = fmap null . readTVar

    processTask :: TQueue Key -> TVar [(Action, Key, [Key])] -> Int -> Key -> IO ()
    processTask queue nodes threadId key = do
      _

    waitForTask :: TQueue Key -> TVar [(Action, Key, [Key])] -> Int -> IO ()
    waitForTask queue nodes threadId = do
      mNextTask <-
        atomically $
          Just <$> readTQueue queue <|> do
            finished <- isFinished nodes
            if finished then pure Nothing else retry
      case mNextTask of
        Nothing ->
          pure ()
        Just nextTask ->
          processTask queue nodes threadId nextTask

run :: (MonadStore Key Object m, MonadIO m) => Key -> m Value
run key = do
  do
    path <- keyPath key
    liftIO . putStrLn $ "running action " <> path

  mOutputKey <- getMemo key
  case mOutputKey of
    Nothing -> do
      liftIO $ putStrLn "  no cached result found, building"

      object <- Maybe.fromMaybe undefined <$> get key
      case object of
        Object.Action action@(Action inputs builderKey) -> do
          dependencyGraph <- getDependencyGraph action

          do
            let (graph, nodeFromVertex, _) = Graph.graphFromEdges dependencyGraph
            liftIO $ putStrLn "  dependencies:"
            traverse_
              ( \vertex -> do
                  let (_, k, _) = nodeFromVertex vertex
                  liftIO . putStrLn . ("  " <>) =<< keyPath k
              )
              (Graph.reverseTopSort graph)

          inputs' <-
            traverse
              ( \(inputName, input) -> do
                  input' <- run input
                  case input' of
                    Value.Object inputKey -> do
                      inputKeyPath <- keyPath inputKey
                      pure (inputName, inputKeyPath)
                    _ ->
                      undefined
              )
              inputs

          builderExecutable <- keyPath builderKey

          liftIO $ do
            p <- Directory.getPermissions builderExecutable
            Directory.setPermissions builderExecutable p {Directory.executable = True}

          liftIO . putStrLn $
            "running builder: "
              <> foldMap (\(name, input) -> Text.unpack name <> "=" <> input <> " ") inputs'
              <> builderExecutable
          (_mhStdin, mhStdout, _mhStderr, processHandle) <-
            liftIO $
              Process.createProcess
                ( ( Process.proc
                      builderExecutable
                      []
                  )
                    { Process.std_in = Process.Inherit,
                      Process.std_out = Process.CreatePipe,
                      Process.std_err = Process.Inherit,
                      Process.env = Just inputs'
                    }
                )

          case mhStdout of
            Nothing ->
              undefined
            Just hStdout -> do
              contents <- liftIO $ do
                contents <- ByteString.hGetContents hStdout
                exitCode <- Process.waitForProcess processHandle
                unless (exitCode == ExitSuccess) . error $ builderExecutable <> " failed"
                pure contents

              outputKey <- put (Object.Object contents)
              putMemo key outputKey
              pure $ Value.Object outputKey
        _ ->
          error $ "expected action, got: " <> show object
    Just outputKey -> do
      outputPath <- keyPath outputKey
      liftIO . putStrLn $ "  cached result found (" <> outputPath <> "), skipping build"
      pure $ Value.Object outputKey

main :: IO ()
main = do
  let env = FileStoreEnv {storePath = "./store", dbPath = "database.json"}
  runFileStoreT env $ do
    action <-
      Eval.eval []
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
                      ("goodbye", Expr.Var "goodbye"),
                      ("cat", Expr.File "/nix/store/xp5z3k851fs7haqbcwqax1hh4pynzla9-coreutils-9.1/bin/cat")
                    ]
                ),
                ("builder", Expr.File "./concat.sh")
              ]
          )

    value <- case action of
      Value.Action key -> run key
      _ -> undefined

    case value of
      Value.Object key -> do
        path <- keyPath key
        liftIO . putStrLn $ "built " <> path
      _ ->
        undefined
module Builder (buildKey, getDependencyGraph, BuildError (..)) where

import Action (Action (..))
import Builder.Logging (BuildLogger (..))
import qualified Data.ByteString as ByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Key (Key)
import Store (Store)
import qualified Store
import qualified System.Directory as Directory
import System.Exit (ExitCode (..))
import qualified System.IO
import qualified System.Process as Process
import Prelude hiding (log)

getDependencyGraph :: Store -> Key -> IO [(Action, Key, [Key])]
getDependencyGraph store key =
  Map.foldrWithKey (\k (a, dependencies) rest -> (a, k, dependencies) : rest) []
    <$> go key
 where
  go :: Key -> IO (Map Key (Action, [Key]))
  go k = do
    mObject <- store.get k
    case mObject of
      Just (Store.Action action@Action{env, args, builder = _}) -> do
        envDeps <- mconcat <$> traverse (go . snd) env
        argsDeps <- mconcat <$> traverse go args
        pure $ Map.insert k (action, snd <$> env) (envDeps <> argsDeps)
      _ ->
        pure mempty

data BuildError = BuildError
  { env :: [(String, String)]
  , args :: [String]
  , builder :: String
  , exitCode :: ExitCode
  }
  deriving (Eq, Show)

buildKey :: Store -> BuildLogger -> Key -> IO (Either BuildError ())
buildKey store buildLogger key = do
  let path = store.keyPath key
  buildLogger.logLine $ "running action " <> path

  mOutputKey <- store.getMemo key
  case mOutputKey of
    Just outputKey -> do
      let outputPath = store.keyPath outputKey
      buildLogger.logLine $ "  cached result found (" <> outputPath <> "), skipping build"
      pure $ Right ()
    Nothing -> do
      buildLogger.logLine "  no cached result found, building"

      object <- Maybe.fromMaybe undefined <$> store.get key
      case object of
        Store.Action Action{env, args, builder} -> do
          env' <-
            traverse
              (\(inputName, input) -> (Text.unpack inputName,) <$> lookupInputResult input)
              env

          args' <-
            traverse
              (\arg -> lookupInputResult arg)
              args

          let builderExecutable = store.keyPath builder
          setPermissionExecutable builderExecutable

          buildLogger.logLine $ "running builder: " <> displayBuilderCommand env' builderExecutable

          (_mhStdin, mhStdout, mhStderr, processHandle) <-
            Process.createProcess
              ( ( Process.proc
                    builderExecutable
                    args'
                )
                  { Process.std_in = Process.Inherit
                  , Process.std_out = Process.CreatePipe
                  , Process.std_err = Process.CreatePipe
                  , Process.env = Just env'
                  }
              )

          case (mhStdout, mhStderr) of
            (Just hStdout, Just hStderr) -> do
              stdout <- ByteString.hGetContents hStdout
              buildLogger.log =<< System.IO.hGetContents hStderr

              exitCode <- Process.waitForProcess processHandle
              if exitCode == ExitSuccess
                then do
                  outputKey <- store.put $ Store.Object stdout
                  store.putMemo key outputKey

                  pure $ Right ()
                else pure . Left $ BuildError{env = env', args = args', builder = builderExecutable, exitCode}
            _ ->
              undefined
        _ ->
          error $ "expected action, got: " <> show object
 where
  setPermissionExecutable path = do
    p <- Directory.getPermissions path
    Directory.setPermissions path p{Directory.executable = True}

  displayBuilderCommand env exe =
    foldMap
      (\(name, input) -> name <> "=" <> input <> " ")
      env
      <> exe

  lookupInputResult input = do
    mInputResultKey <- store.getMemo input
    case mInputResultKey of
      Nothing -> do
        let inputPath = store.keyPath input
        error $ "no result found for " <> inputPath
      Just inputResultKey ->
        pure $ store.keyPath inputResultKey
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Strict (execStateT, modify)
import Control.Monad.Trans (lift)
import qualified Crypto.Hash.SHA256 as Sha256
import Data.Aeson (FromJSON, ToJSON, toJSON)
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base32 as Base32
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Foldable (traverse_)
import qualified Data.Graph as Graph
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Generics (Generic)
import qualified System.Directory as Directory
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.Process as Process

newtype Base32 = Base32 {unBase32 :: ByteString}
  deriving (Eq, Ord, Show)

toText :: Base32 -> Text
toText = Text.toLower . Base32.encodeBase32Unpadded . unBase32

toString :: Base32 -> String
toString = Text.unpack . toText

instance ToJSON Base32 where
  toJSON = Json.toJSON @Text . toText

instance FromJSON Base32 where
  parseJSON value = do
    text <- Json.parseJSON @Text value
    case Base32.decodeBase32Unpadded $ Text.Encoding.encodeUtf8 text of
      Left err ->
        fail $ Text.unpack err
      Right bytes ->
        pure $ Base32 bytes

data Key
  = ActionKey Base32
  | ObjectKey Base32
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Key

instance FromJSON Key

class Monad m => MonadStore key value m | m -> key value where
  keyPath :: Key -> m FilePath

  get :: key -> m (Maybe value)
  put :: value -> m key
  delete :: key -> m Bool

  getMemo :: Key -> m (Maybe Key)
  putMemo :: Key -> Key -> m ()

data Action = Action {aInputs :: [(String, Key)], aBuilder :: Key}
  deriving (Eq, Show, Generic)

instance ToJSON Action

instance FromJSON Action

data Object
  = OAction Action
  | OObject ByteString
  deriving (Eq, Show)

hashObject :: MonadStore key value m => Object -> m ByteString
hashObject (OAction (Action inputs builder)) =
  fmap Sha256.finalize . flip execStateT Sha256.init $ do
    traverse_
      ( \(inputName, input) -> do
          modify $ \ctx -> Sha256.update ctx (ByteString.Char8.pack inputName)
          inputPath <- lift $ keyPath input
          modify $ \ctx -> Sha256.update ctx (ByteString.Char8.pack inputPath)
      )
      inputs
    builderPath <- lift $ keyPath builder
    modify $ \ctx -> Sha256.update ctx (ByteString.Char8.pack builderPath)
hashObject (OObject contents) = pure $ Sha256.hash contents

data Expr
  = EVar String
  | ELam String Expr
  | EApp Expr Expr
  | ERecord [(String, Expr)]
  | EProject Expr String
  | {-
    inputs : { f_0 : Action T_0, f_1 : Action T_1, ..., f_n : Action T_n }
    builder : { f_0 : T_0, f_1 : T_1, ..., f_n : T_n } -> output
    --------------------------
    action { inputs, builder } : output
    -}
    EAction Expr
  | EFile FilePath
  | ELet String Expr Expr
  deriving (Eq, Show)

data Value
  = VAction Key
  | VObject Key
  | VLam [(String, Value)] String Expr
  | VRecord [(String, Value)]
  deriving (Eq, Show)

getDependencyGraph :: MonadStore Key Object m => Action -> m (Map Key (Action, [Key]))
getDependencyGraph (Action inputs _) =
  mconcat
    <$> traverse
      ( \(_, input) -> do
          mObject <- get input
          case mObject of
            Just (OAction action) ->
              Map.insert input (action, snd <$> aInputs action) <$> getDependencyGraph action
            _ ->
              pure mempty
      )
      inputs

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
        OAction action@(Action inputs builderKey) -> do
          (dependencyGraph, nodeFromVertex, _vertexFromKey) <-
            Graph.graphFromEdges
              . Map.foldrWithKey (\k (v1, v2) rest -> (v1, k, v2) : rest) []
              <$> getDependencyGraph action

          let -- actions that have no dependencies
              roots =
                filter
                  ( \vertex ->
                      let (_, _, edges) = nodeFromVertex vertex
                       in null edges
                  )
                  . Graph.vertices
                  $
                  -- A -> B when A depends on B
                  dependencyGraph

          do
            liftIO $ putStrLn "  dependencies:"
            traverse_
              ( \vertex -> do
                  let (_, k, _) = nodeFromVertex vertex
                  liftIO . putStrLn . ("  " <>) =<< keyPath k
              )
              (Graph.reverseTopSort dependencyGraph)

          inputs' <-
            traverse
              ( \(inputName, input) -> do
                  input' <- run input
                  case input' of
                    VObject inputKey -> do
                      inputKeyPath <- keyPath inputKey
                      pure (inputName, inputKeyPath)
                    _ ->
                      undefined
              )
              inputs

          builderExecutable <- keyPath builderKey

          liftIO $ do
            p <- Directory.getPermissions builderExecutable
            Directory.setPermissions builderExecutable p{Directory.executable = True}

          liftIO . putStrLn $
            "running builder: "
              <> foldMap (\(name, input) -> name <> "=" <> input <> " ") inputs'
              <> builderExecutable
          (_mhStdin, mhStdout, _mhStderr, processHandle) <-
            liftIO $
              Process.createProcess
                ( ( Process.proc
                      builderExecutable
                      []
                  )
                    { Process.std_in = Process.Inherit
                    , Process.std_out = Process.CreatePipe
                    , Process.std_err = Process.Inherit
                    , Process.env = Just inputs'
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

              outputKey <- put (OObject contents)
              putMemo key outputKey
              pure $ VObject outputKey
        _ ->
          error $ "expected action, got: " <> show object
    Just outputKey -> do
      outputPath <- keyPath outputKey
      liftIO . putStrLn $ "  cached result found (" <> outputPath <> "), skipping build"
      pure $ VObject outputKey

eval :: (MonadStore Key Object m, MonadIO m) => [(String, Value)] -> Expr -> m Value
eval ctx expr =
  case expr of
    EVar var ->
      pure . Maybe.fromMaybe undefined $ lookup var ctx
    ELam name body ->
      pure $ VLam ctx name body
    ELet name value body -> do
      value' <- eval ctx value
      eval ((name, value') : ctx) body
    EApp f x -> do
      f' <- eval ctx f
      x' <- eval ctx x
      case f' of
        VLam ctx' name body ->
          eval ((name, x') : ctx') body
        _ ->
          undefined
    ERecord fields -> do
      fields' <- (traverse . traverse) (eval ctx) fields
      pure $ VRecord fields'
    EProject record field -> do
      record' <- eval ctx record
      case record' of
        VRecord fields ->
          pure . Maybe.fromMaybe undefined $ lookup field fields
        _ ->
          undefined
    EAction args -> do
      args' <- eval ctx args
      case args' of
        VRecord [("inputs", VRecord inputs), ("builder", builder)] -> do
          inputs' <-
            traverse
              ( \(inputName, input) -> case input of
                  VAction key -> pure (inputName, key)
                  _ -> undefined
              )
              inputs
          builder' <-
            case builder of
              VObject key ->
                pure key
              _ ->
                undefined
          VAction <$> put (OAction $ Action inputs' builder')
        _ ->
          undefined
    EFile filePath -> do
      contents <- liftIO $ ByteString.readFile filePath
      VObject <$> put (OObject contents)

data FileStoreEnv = FileStoreEnv {storePath :: FilePath, dbPath :: FilePath}

newtype FileStoreT m a = FileStoreT (ReaderT FileStoreEnv m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadStore Key Object (FileStoreT m) where
  keyPath (ActionKey key) = FileStoreT $ asks (flip FilePath.replaceExtension "action" . (</> toString key) . storePath)
  keyPath (ObjectKey key) = FileStoreT $ asks (flip FilePath.replaceExtension "object" . (</> toString key) . storePath)

  get key = do
    path <- keyPath key
    exists <- liftIO $ Directory.doesFileExist path
    if exists
      then do
        liftIO $ case key of
          ActionKey _ -> do
            action <- either error pure =<< Json.eitherDecodeFileStrict @Action path
            pure . Just $ OAction action
          ObjectKey _ -> Just . OObject <$> ByteString.readFile path
      else pure Nothing

  put object = do
    hash <- hashObject object
    let key =
          case object of
            OAction _ -> ActionKey $ Base32 hash
            OObject _ -> ObjectKey $ Base32 hash
    path <- keyPath key
    exists <- liftIO $ Directory.doesFileExist path
    unless exists . liftIO $ case object of
      OAction action ->
        Json.encodeFile path action
      OObject contents ->
        ByteString.writeFile path contents
    pure key

  delete key = do
    path <- keyPath key
    liftIO $ do
      exists <- Directory.doesFileExist path
      when exists $ Directory.removeFile path
      pure exists

  getMemo key = do
    database <- do
      path <- FileStoreT $ asks dbPath
      liftIO $ either error pure =<< Json.eitherDecodeFileStrict @[(Key, Key)] path
    case lookup key database of
      Just value ->
        pure $ Just value
      Nothing ->
        pure Nothing

  putMemo key value = do
    path <- FileStoreT $ asks dbPath
    liftIO $ do
      database <- either error pure =<< Json.eitherDecodeFileStrict @[(Key, Key)] path
      Json.encodeFile path ((key, value) : database)

runFileStoreT :: MonadIO m => FileStoreEnv -> FileStoreT m a -> m a
runFileStoreT env (FileStoreT m) = do
  liftIO $ do
    dbExists <- Directory.doesFileExist (dbPath env)
    unless dbExists $ Json.encodeFile @[(Key, Key)] (dbPath env) []

    storeExists <- Directory.doesDirectoryExist (storePath env)
    unless storeExists $ Directory.createDirectory (storePath env)

  runReaderT m env

main :: IO ()
main = do
  let env = FileStoreEnv{storePath = "./store", dbPath = "database.json"}
  runFileStoreT env $ do
    action <-
      eval [] $
        {-
          action {
            inputs = {}
            builder = file "./builder.sh"
          }
        -}
        ELet
          "hello"
          ( EAction
              ( ERecord
                  [ ("inputs", ERecord [])
                  , ("builder", EFile "./hello.sh")
                  ]
              )
          )
          . ELet
            "goodbye"
            ( EAction
                ( ERecord
                    [ ("inputs", ERecord [])
                    , ("builder", EFile "./goodbye.sh")
                    ]
                )
            )
          $ EAction
            ( ERecord
                [
                  ( "inputs"
                  , ERecord
                      [ ("hello", EVar "hello")
                      , ("goodbye", EVar "goodbye")
                      , ("cat", EFile "/nix/store/xp5z3k851fs7haqbcwqax1hh4pynzla9-coreutils-9.1/bin/cat")
                      ]
                  )
                , ("builder", EFile "./concat.sh")
                ]
            )

    value <- case action of
      VAction key -> run key
      _ -> undefined

    case value of
      VObject key -> do
        path <- keyPath key
        liftIO . putStrLn $ "built " <> path
      _ ->
        undefined
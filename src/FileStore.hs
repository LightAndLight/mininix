module FileStore (FileStoreT, FileStoreEnv (..), runFileStoreT) where

import Action (Action)
import Base32 (Base32 (..))
import qualified Base32
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import qualified Data.Aeson as Json
import qualified Data.ByteString as ByteString
import Key (Key (..))
import qualified Key
import Object (Object)
import qualified Object
import Store (MonadStore (..))
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

data FileStoreEnv = FileStoreEnv {storePath :: FilePath, dbPath :: FilePath}

newtype FileStoreT m a = FileStoreT (ReaderT FileStoreEnv m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadStore Key Object (FileStoreT m) where
  keyPath (Key Key.Action key) = FileStoreT $ asks (flip FilePath.replaceExtension "action" . (</> Base32.toString key) . (.storePath))
  keyPath (Key Key.Object key) = FileStoreT $ asks (flip FilePath.replaceExtension "object" . (</> Base32.toString key) . (.storePath))

  get key = do
    path <- keyPath key
    exists <- liftIO $ Directory.doesFileExist path
    if exists
      then do
        liftIO $ case key of
          Key Key.Action _ -> do
            action <- either error pure =<< Json.eitherDecodeFileStrict @Action path
            pure . Just $ Object.Action action
          Key Key.Object _ -> Just . Object.Object <$> ByteString.readFile path
      else pure Nothing

  put object = do
    hash <- Object.hash object
    let key =
          case object of
            Object.Action _ -> Key Key.Action $ Base32 hash
            Object.Object _ -> Key Key.Object $ Base32 hash
    path <- keyPath key
    exists <- liftIO $ Directory.doesFileExist path
    unless exists . liftIO $ case object of
      Object.Action action ->
        Json.encodeFile path action
      Object.Object contents ->
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
      path <- FileStoreT $ asks (.dbPath)
      liftIO $ either error pure =<< Json.eitherDecodeFileStrict @[(Key, Key)] path
    case lookup key database of
      Just value ->
        pure $ Just value
      Nothing ->
        pure Nothing

  putMemo key value = do
    path <- FileStoreT $ asks (.dbPath)
    liftIO $ do
      database <- either error pure =<< Json.eitherDecodeFileStrict @[(Key, Key)] path
      Json.encodeFile path ((key, value) : database)

runFileStoreT :: MonadIO m => FileStoreEnv -> FileStoreT m a -> m a
runFileStoreT env (FileStoreT m) = do
  liftIO $ do
    dbExists <- Directory.doesFileExist env.dbPath
    unless dbExists $ Json.encodeFile @[(Key, Key)] env.dbPath []

    storeExists <- Directory.doesDirectoryExist env.storePath
    unless storeExists $ Directory.createDirectory env.storePath

  runReaderT m env
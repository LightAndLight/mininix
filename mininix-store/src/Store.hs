module Store (Store (..), new, Object (..), hashObject) where

import Action (Action (args, builder, env))
import qualified Action
import Base32 (Base32 (..))
import qualified Base32
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (modify)
import Control.Monad.State.Strict (execState)
import qualified Crypto.Hash.SHA256 as Sha256
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Foldable (traverse_)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Database.SQLite.Simple as Sqlite
import Key (Key (..))
import qualified Key
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

data Store = Store
  { keyPath :: Key -> FilePath
  , get :: Key -> IO (Maybe Object)
  , put :: Object -> IO Key
  , delete :: Key -> IO Bool
  , getMemo :: Key -> IO (Maybe Key)
  , putMemo :: Key -> Key -> IO ()
  }

new :: FilePath -> FilePath -> IO Store
new storePath dbPath = do
  dbExists <- Directory.doesFileExist dbPath
  unless dbExists $
    Sqlite.withConnection dbPath $ \conn ->
      Sqlite.execute_ conn "CREATE TABLE action_outputs (action_hash TEXT PRIMARY KEY, output_hash TEXT NOT NULL)"

  storeExists <- Directory.doesDirectoryExist storePath
  unless storeExists $ Directory.createDirectory storePath

  storeWriteLock <- newMVar ()

  let self :: Store
      self = Store{keyPath, get, put, delete, getMemo, putMemo}

      keyPath :: Key -> FilePath
      keyPath key =
        case key of
          Key Key.Action hash -> FilePath.replaceExtension (storePath </> Base32.toString hash) "action"
          Key Key.Object hash -> FilePath.replaceExtension (storePath </> Base32.toString hash) "object"

      get :: Key -> IO (Maybe Object)
      get key = do
        let path = keyPath key
        exists <- liftIO $ Directory.doesFileExist path
        if exists
          then do
            liftIO $ case key of
              Key Key.Action _ -> do
                result <- Json.eitherDecodeFileStrict @Action path
                action <- case result of
                  Left err -> error $ "failed to decode " <> path <> ": " <> err
                  Right action -> pure action
                pure . Just $ Action action
              Key Key.Object _ -> Just . Object <$> ByteString.readFile path
          else pure Nothing

      put :: Object -> IO Key
      put object = do
        takeMVar storeWriteLock

        let hash = hashObject self object
        let key =
              case object of
                Action _ -> Key Key.Action $ Base32 hash
                Object _ -> Key Key.Object $ Base32 hash
        let path = keyPath key
        exists <- liftIO $ Directory.doesFileExist path
        unless exists . liftIO $ case object of
          Action action ->
            Json.encodeFile path action
          Object contents ->
            ByteString.writeFile path contents

        putMVar storeWriteLock ()

        pure key

      delete :: Key -> IO Bool
      delete key = do
        takeMVar storeWriteLock

        let path = keyPath key
        exists <- Directory.doesFileExist path
        when exists $ Directory.removeFile path

        putMVar storeWriteLock ()

        pure exists

      getMemo :: Key -> IO (Maybe Key)
      getMemo key = do
        results <- Sqlite.withConnection dbPath $ \conn ->
          Sqlite.query conn "SELECT output_hash FROM action_outputs WHERE action_hash = ?" (Sqlite.Only key.hash)
        case results of
          [] ->
            pure Nothing
          [Sqlite.Only (outputHash :: Base32)] ->
            pure . Just $ Key Key.Object outputHash
          _ ->
            undefined

      putMemo :: Key -> Key -> IO ()
      putMemo actionKey outputKey = do
        Sqlite.withConnection dbPath $ \conn ->
          Sqlite.execute conn "INSERT INTO action_outputs (action_hash, output_hash) VALUES (?, ?)" (actionKey.hash, outputKey.hash)

  pure self

data Object
  = Action Action
  | Object ByteString
  deriving (Eq, Show)

hashObject :: Store -> Object -> ByteString
hashObject store object =
  case object of
    Action Action.Action{env, args, builder} ->
      Sha256.finalize . flip execState Sha256.init $ do
        traverse_
          ( \(inputName, input) -> do
              modify $ \ctx -> Sha256.update ctx (Text.Encoding.encodeUtf8 inputName)

              let inputPath = store.keyPath input
              modify $ \ctx -> Sha256.update ctx (ByteString.Char8.pack inputPath)
          )
          env

        traverse_
          ( \arg -> do
              let inputPath = store.keyPath arg
              modify $ \ctx -> Sha256.update ctx (ByteString.Char8.pack inputPath)
          )
          args

        let builderPath = store.keyPath builder
        modify $ \ctx -> Sha256.update ctx (ByteString.Char8.pack builderPath)
    Object contents ->
      Sha256.hash contents

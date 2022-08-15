module Object (Object (..), hash) where

import Action (Action)
import qualified Action
import Control.Monad.State.Class (modify)
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Trans (lift)
import qualified Crypto.Hash.SHA256 as Sha256
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Foldable (traverse_)
import qualified Data.Text.Encoding as Text.Encoding
import Store (MonadStore, keyPath)

data Object
  = Action Action
  | Object ByteString
  deriving (Eq, Show)

hash :: MonadStore key value m => Object -> m ByteString
hash (Action (Action.Action inputs builder)) =
  fmap Sha256.finalize . flip execStateT Sha256.init $ do
    traverse_
      ( \(inputName, input) -> do
          modify $ \ctx -> Sha256.update ctx (Text.Encoding.encodeUtf8 inputName)
          inputPath <- lift $ keyPath input
          modify $ \ctx -> Sha256.update ctx (ByteString.Char8.pack inputPath)
      )
      inputs
    builderPath <- lift $ keyPath builder
    modify $ \ctx -> Sha256.update ctx (ByteString.Char8.pack builderPath)
hash (Object contents) = pure $ Sha256.hash contents
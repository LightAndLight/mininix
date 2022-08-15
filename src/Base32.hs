module Base32 (Base32 (..), toText, toString) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base32 as Base32
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding

newtype Base32 = Base32 {value :: ByteString}
  deriving (Eq, Ord, Show)

toText :: Base32 -> Text
toText = Text.toLower . Base32.encodeBase32Unpadded . (.value)

toString :: Base32 -> String
toString = Text.unpack . toText

instance ToJSON Base32 where
  toJSON = toJSON @Text . toText

instance FromJSON Base32 where
  parseJSON value = do
    text <- parseJSON @Text value
    case Base32.decodeBase32Unpadded $ Text.Encoding.encodeUtf8 text of
      Left err ->
        fail $ Text.unpack err
      Right bytes ->
        pure $ Base32 bytes
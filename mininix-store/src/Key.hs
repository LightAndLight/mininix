module Key (Key (..), KeyType (..)) where

import Base32 (Base32)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data KeyType
  = Action
  | Object
  deriving (Eq, Ord, Show, Generic)

instance ToJSON KeyType

instance FromJSON KeyType

data Key = Key {type_ :: KeyType, hash :: Base32}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Key

instance FromJSON Key
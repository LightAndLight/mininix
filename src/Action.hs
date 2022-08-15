module Action (Action (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Key (Key)

data Action = Action {inputs :: [(Text, Key)], builder :: Key}
  deriving (Eq, Show, Generic)

instance ToJSON Action

instance FromJSON Action
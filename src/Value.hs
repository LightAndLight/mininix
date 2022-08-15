module Value (Value (..)) where

import Data.Text (Text)
import Expr (Expr)
import Key (Key)

data Value
  = Action Key
  | Object Key
  | Lam [(Text, Value)] Text Expr
  | Record [(Text, Value)]
  deriving (Eq, Show)
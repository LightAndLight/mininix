module Type (Type (..)) where

import Data.Text (Text)

data Type
  = TArrow Type Type
  | TRecord [(Text, Type)]
  | TString
  | TAction
  | TArtifact
  | TArray Type
  deriving (Eq, Show)
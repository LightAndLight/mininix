module Syntax (Expr (..)) where

import Data.Text (Text)
import Type (Type)

data Expr
  = Var Text
  | Lam Text Type Expr
  | App Expr Expr
  | Record [(Text, Expr)]
  | Project Expr Text
  | String Text
  | Let Text Expr Expr
  | Array [Expr] Type
  deriving (Eq, Show)
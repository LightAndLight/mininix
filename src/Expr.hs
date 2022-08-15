module Expr (Expr (..)) where

import Data.Text (Text)

data Expr
  = Var Text
  | Lam Text Expr
  | App Expr Expr
  | Record [(Text, Expr)]
  | Project Expr Text
  | {-
    inputs : { f_0 : Action T_0, f_1 : Action T_1, ..., f_n : Action T_n }
    builder : { f_0 : T_0, f_1 : T_1, ..., f_n : T_n } -> output
    --------------------------
    action { inputs, builder } : output
    -}
    Action Expr
  | File FilePath
  | Let Text Expr Expr
  deriving (Eq, Show)
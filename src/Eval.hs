module Eval (eval) where

import Action (Action (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Expr (Expr)
import qualified Expr
import Key (Key)
import Object (Object)
import qualified Object
import Store (MonadStore, put)
import Value (Value)
import qualified Value

eval :: (MonadStore Key Object m, MonadIO m) => [(Text, Value)] -> Expr -> m Value
eval ctx expr =
  case expr of
    Expr.Var var ->
      pure . Maybe.fromMaybe undefined $ lookup var ctx
    Expr.Lam name body ->
      pure $ Value.Lam ctx name body
    Expr.Let name value body -> do
      value' <- eval ctx value
      eval ((name, value') : ctx) body
    Expr.App f x -> do
      f' <- eval ctx f
      x' <- eval ctx x
      case f' of
        Value.Lam ctx' name body ->
          eval ((name, x') : ctx') body
        _ ->
          undefined
    Expr.Record fields -> do
      fields' <- (traverse . traverse) (eval ctx) fields
      pure $ Value.Record fields'
    Expr.Project record field -> do
      record' <- eval ctx record
      case record' of
        Value.Record fields ->
          pure . Maybe.fromMaybe undefined $ lookup field fields
        _ ->
          undefined
    Expr.Action args -> do
      args' <- eval ctx args
      case args' of
        Value.Record [("inputs", Value.Record inputs), ("builder", builder)] -> do
          inputs' <-
            traverse
              ( \(inputName, input) -> case input of
                  Value.Action key -> pure (inputName, key)
                  _ -> undefined
              )
              inputs
          builder' <-
            case builder of
              Value.Object key ->
                pure key
              _ ->
                undefined
          Value.Action <$> put (Object.Action $ Action inputs' builder')
        _ ->
          undefined
    Expr.File filePath -> do
      contents <- liftIO $ ByteString.readFile filePath
      Value.Object <$> put (Object.Object contents)
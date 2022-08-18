module Eval (eval) where

import Action (Action (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Expr (Expr)
import qualified Expr
import Store (Store, put)
import qualified Store
import Value (Value)
import qualified Value

eval :: MonadIO m => Store -> [(Text, Value)] -> Expr -> m Value
eval store ctx expr =
  case expr of
    Expr.Var var ->
      pure . Maybe.fromMaybe undefined $ lookup var ctx
    Expr.Lam name body ->
      pure $ Value.Lam ctx name body
    Expr.String str ->
      pure $ Value.String str
    Expr.Let name value body -> do
      value' <- eval store ctx value
      eval store ((name, value') : ctx) body
    Expr.App f x -> do
      f' <- eval store ctx f
      x' <- eval store ctx x
      case f' of
        Value.Lam ctx' name body ->
          eval store ((name, x') : ctx') body
        _ ->
          undefined
    Expr.Record fields -> do
      fields' <- (traverse . traverse) (eval store ctx) fields
      pure $ Value.Record fields'
    Expr.Project record field -> do
      record' <- eval store ctx record
      case record' of
        Value.Record fields ->
          pure . Maybe.fromMaybe undefined $ lookup field fields
        _ ->
          undefined
    Expr.Action action -> do
      action' <- eval store ctx action
      case action' of
        Value.Record [("env", Value.Record env), ("args", Value.Array args), ("builder", builder)] -> do
          env' <-
            traverse
              ( \(inputName, input) -> case input of
                  Value.Action key -> pure (inputName, key)
                  _ -> undefined
              )
              env
          args' <-
            traverse
              ( \arg -> case arg of
                  Value.Action key -> pure key
                  _ -> undefined
              )
              args
          builder' <-
            case builder of
              Value.Object key ->
                pure key
              _ ->
                undefined
          liftIO $ Value.Action <$> store.put (Store.Action $ Action{env = env', args = args', builder = builder'})
        _ ->
          undefined
    Expr.File path -> do
      path' <- eval store ctx path
      case path' of
        Value.String filePath -> do
          contents <- liftIO . ByteString.readFile $ Text.unpack filePath
          liftIO $ Value.Object <$> store.put (Store.Object contents)
        _ ->
          error $ "expected string, got " <> show path'
    Expr.Array items -> do
      items' <- traverse (eval store ctx) items
      pure $ Value.Array items'
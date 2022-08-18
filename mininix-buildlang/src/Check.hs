module Check (check) where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Text (Text)
import qualified Expr
import qualified Syntax
import Type (Type (..))

data Error
  = NotInScope Text
  | ExpectedFunction {actual :: Type}
  | ExpectedRecord {actual :: Type}
  | ExpectedAction {actual :: Type}
  | RecordMissingField {record :: Type, field :: Text}
  | ActionMissingArgument
  | FileMissingArgument
  | TypeMismatch {expected :: Type, actual :: Type}
  | InvalidArguments {expectedArguments :: [Text], actualArguments :: [Text]}
  | Can'tInferArray
  deriving (Eq, Show)

check :: HashMap Text Type -> Syntax.Expr -> Type -> Either Error Expr.Expr
check ctx expr expectedTy = do
  (expr', actualTy) <- infer ctx expr
  if expectedTy == actualTy
    then pure expr'
    else Left $ TypeMismatch{expected = expectedTy, actual = actualTy}

infer :: HashMap Text Type -> Syntax.Expr -> Either Error (Expr.Expr, Type)
infer ctx expr =
  case expr of
    Syntax.Var var ->
      case var of
        "action" ->
          Left ActionMissingArgument
        "file" ->
          Left FileMissingArgument
        _ ->
          case HashMap.lookup var ctx of
            Nothing ->
              Left $ NotInScope var
            Just ty ->
              pure (Expr.Var var, ty)
    Syntax.Lam name ty body -> do
      (body', outTy) <- infer (HashMap.insert name ty ctx) body
      pure (Expr.Lam name body', TArrow ty outTy)
    Syntax.App f x ->
      case f of
        Syntax.Var "file" -> do
          x' <- check ctx x TString
          pure (Expr.File x', TArtifact)
        Syntax.Var "action" -> do
          (x', xTy) <- infer ctx x

          fields <- case xTy of
            TRecord fields ->
              pure fields
            _ -> Left $ ExpectedRecord{actual = xTy}

          (envTy, argsTy, builderTy) <- case fields of
            [("env", envTy), ("args", argsTy), ("builder", builderTy)] ->
              pure (envTy, argsTy, builderTy)
            _ ->
              Left $
                InvalidArguments
                  { expectedArguments = ["env", "args", "builder"]
                  , actualArguments = fst <$> fields
                  }

          env <- case envTy of
            TRecord env ->
              pure env
            _ ->
              Left $ ExpectedRecord{actual = envTy}

          traverse_
            ( \(_, input) ->
                unless (input == TAction) $ Left ExpectedAction{actual = input}
            )
            env

          unless (argsTy == TArray TString) $ Left TypeMismatch{expected = TArray TString, actual = envTy}

          unless (builderTy == TArtifact) $ Left TypeMismatch{expected = TArtifact, actual = builderTy}

          pure (Expr.Action x', TAction)
        _ -> do
          (f', fTy) <- infer ctx f
          case fTy of
            TArrow inTy outTy -> do
              x' <- check ctx x inTy
              pure (Expr.App f' x', outTy)
            _ ->
              Left $ ExpectedFunction fTy
    Syntax.Record fields -> do
      (fields', fieldTys) <-
        List.unzip
          <$> traverse
            ( \(field, fieldExpr) -> do
                (fieldExpr', fieldTy) <- infer ctx fieldExpr
                pure ((field, fieldExpr'), (field, fieldTy))
            )
            fields
      pure (Expr.Record fields', TRecord fieldTys)
    Syntax.Project record field -> do
      (record', recordTy) <- infer ctx record
      case recordTy of
        TRecord fields ->
          case lookup field fields of
            Nothing ->
              Left $ RecordMissingField{record = recordTy, field}
            Just ty ->
              pure (Expr.Project record' field, ty)
        _ -> Left $ ExpectedRecord{actual = recordTy}
    Syntax.String str ->
      pure (Expr.String str, TString)
    Syntax.Let name value rest -> do
      (value', valueTy) <- infer ctx value
      (rest', restTy) <- infer (HashMap.insert name valueTy ctx) rest
      pure (Expr.Let name value' rest', restTy)
    Syntax.Array items itemTy -> do
      items' <- traverse (\item -> check ctx item itemTy) items
      pure (Expr.Array items', TArray itemTy)

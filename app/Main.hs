{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Builder (buildKey, getDependencyGraph)
import Builder.Parallel (buildInParallel)
import qualified Eval
import qualified Expr
import Key (Key)
import Store (Store)
import qualified Store
import qualified Value
import Prelude hiding (log)

build :: Store -> Key -> IO Key
build store key = do
  let path = store.keyPath key
  putStrLn $ "running action " <> path

  mOutputKey <- store.getMemo key
  case mOutputKey of
    Nothing -> do
      putStrLn "  no cached result found, building"

      dependencyGraph <- getDependencyGraph store key
      buildInParallel (buildKey store) dependencyGraph

      maybe undefined pure =<< store.getMemo key
    Just outputKey -> do
      let outputPath = store.keyPath outputKey
      putStrLn $ "  cached result found (" <> outputPath <> "), skipping build"
      pure outputKey

main :: IO ()
main = do
  store <- Store.new "./store" "database.json"
  action <-
    Eval.eval store [] $
      {-
        action {
          inputs = {}
          builder = file "./builder.sh"
        }
      -}
      Expr.Let
        "hello"
        ( Expr.Action
            ( Expr.Record
                [ ("inputs", Expr.Record [])
                , ("builder", Expr.File "./hello.sh")
                ]
            )
        )
        . Expr.Let
          "goodbye"
          ( Expr.Action
              ( Expr.Record
                  [ ("inputs", Expr.Record [])
                  , ("builder", Expr.File "./goodbye.sh")
                  ]
              )
          )
        $ Expr.Action
          ( Expr.Record
              [
                ( "inputs"
                , Expr.Record
                    [ ("hello", Expr.Var "hello")
                    , ("goodbye", Expr.Var "goodbye")
                    ]
                )
              , ("builder", Expr.File "./concat.sh")
              ]
          )

  case action of
    Value.Action key -> do
      outputKey <- build store key
      putStrLn $ "built " <> store.keyPath outputKey
    _ -> undefined
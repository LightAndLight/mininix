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
import Check (check)
import qualified Eval
import Key (Key)
import qualified Parser
import Store (Store)
import qualified Store
import System.Exit (exitFailure)
import Text.Trifecta (ErrInfo (_errDoc), Result (..), parseFromFileEx)
import Type (Type (..))
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
  store <- Store.new "./store" "./database.sqlite"
  result <- parseFromFileEx Parser.expr "test.mini"

  syntax <- case result of
    Failure err -> do
      print err._errDoc
      exitFailure
    Success syntax ->
      pure syntax

  expr <- case check mempty syntax TAction of
    Left err -> error $ show err
    Right expr -> pure expr

  action <- Eval.eval store [] expr

  key <- case action of
    Value.Action key ->
      pure key
    _ -> undefined

  outputKey <- build store key
  putStrLn $ "built " <> store.keyPath outputKey
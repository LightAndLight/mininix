{-# LANGUAGE OverloadedLists #-}

module Parser (expr, type_) where

import Control.Applicative (many, (<|>))
import Data.Text (Text)
import Syntax (Expr (..))
import Text.Parser.Char (alphaNum, char, lower)
import Text.Parser.Combinators (sepBy)
import Text.Parser.Token (IdentifierStyle (..), TokenParsing, braces, comma, parens, stringLiteral, symbol, symbolic)
import qualified Text.Parser.Token as Token
import qualified Text.Parser.Token.Highlight as Highlight
import Type (Type (..))

identStyle :: (Monad m, TokenParsing m) => IdentifierStyle m
identStyle =
  IdentifierStyle
    { _styleName = "ident"
    , _styleStart = lower
    , _styleLetter = alphaNum
    , _styleReserved = ["let", "in"]
    , _styleHighlight = Highlight.Identifier
    , _styleReservedHighlight = Highlight.ReservedIdentifier
    }

ident :: (Monad m, TokenParsing m) => m Text
ident =
  Token.ident identStyle

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved =
  Token.reserve identStyle

expr :: (Monad m, TokenParsing m) => m Expr
expr =
  uncurry Lam <$ char '\\' <*> parens ((,) <$> ident <* symbolic ':' <*> type_) <* symbol "->" <*> expr
    <|> Let <$ reserved "let" <*> ident <* symbolic '=' <*> expr <* reserved "in" <*> expr
    <|> foldl App <$> project <*> many project
 where
  project =
    foldl Project <$> atom <*> many ident

  atom =
    Var <$> ident
      <|> Record <$> braces (recordField `sepBy` comma)
      <|> String <$> stringLiteral

  recordField =
    (,) <$> ident <* symbolic '=' <*> expr

type_ :: (Monad m, TokenParsing m) => m Type
type_ =
  foldr TArrow <$> atom <*> many (symbol "->" *> atom)
 where
  atom =
    TRecord <$> braces (recordField `sepBy` comma)
      <|> TString <$ symbol "String"
      <|> TAction <$ symbol "Action"
      <|> TArtifact <$ symbol "Artifact"

  recordField =
    (,) <$> ident <* symbolic '=' <*> type_
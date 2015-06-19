module Language.Haskell.Expression (parseExpression, parseType) where

import           Language.Haskell.TH.Syntax

import           Data.Char
import           Text.Parsec
import           Control.Applicative ((<$>))

import           Language.Haskell.Expression.Lexer

parseExpression :: String -> Either String Exp
parseExpression input = case parse (contents expParser) "foo.hs" input of
  Left err -> (Left $ show err)
  Right a -> Right a

parseType :: String -> Either String Type
parseType input = case parse (contents typ) "foo.hs" input of
  Left err -> (Left $ show err)
  Right a -> Right a


module Language.Haskell.Expression (parseExpression) where

import           Language.Haskell.TH.Syntax

import           Data.Char
import           Text.Parsec
import           Control.Applicative ((<$>))

import           Language.Haskell.Expression.Lexer

parseExpression :: String -> Either String Exp
parseExpression input = case parse (contents expParser) "foo.hs" input of
  Left err -> (Left $ show err)
  Right a -> Right a

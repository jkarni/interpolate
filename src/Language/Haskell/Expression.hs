module Language.Haskell.Expression (parseExpression, parseType, parsePat) where

import           Language.Haskell.TH.Syntax

import           Data.Char
import           Text.Parsec
import           Control.Applicative ((<$>))

import           Language.Haskell.Expression.Lexer

parseExpression :: String -> Either String Exp
parseExpression = wrapParser expParser

parseType :: String -> Either String Type
parseType = wrapParser typ

parsePat :: String -> Either String Pat
parsePat = wrapParser pat


wrapParser :: SParser () a -> String -> Either String a
wrapParser parser input = case parse (contents parser) "foo.hs" input of
  Left err -> (Left $ show err)
  Right a -> Right a


contents :: SParser u a -> SParser u a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

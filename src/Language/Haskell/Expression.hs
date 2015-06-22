module Language.Haskell.Expression (parseExp, parseType, parsePat) where

import           Language.Haskell.TH.Syntax
import           Text.Parsec

import           Language.Haskell.Expression.Parser

parseExp :: String -> Either String Exp
parseExp = wrapParser expParser

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

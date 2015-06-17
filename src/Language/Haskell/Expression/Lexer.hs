{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Expression.Lexer where

import Data.Char
import Text.Parsec
import Text.Parsec.Language (haskell, haskellStyle)
import qualified Text.Parsec.Token as P
import Language.Haskell.TH.Syntax

-- TODO: Rename this file

contents :: Parsec String u Exp -> Parsec String u Exp
contents p = do
  P.whiteSpace haskell
  r <- p
  eof
  return r

-- Remove left recursion
expParser :: Parsec String u Exp
expParser = try (app <?> "function application")
        <|> try sig
        <|> try (lit <?> "literal")
        <|> try (lambdaabs <?> "lambda")
        <|> try (var <?> "variable")

app :: Parsec String u Exp
app = do
  fn <- var <|> lambdaabs
  P.whiteSpace haskell
  v  <- var <|> lit
  return $! AppE fn v

lambdaabs :: Parsec String u Exp
lambdaabs = do
  char '\\'
  P.whiteSpace haskell
  p <- many1 apat
  P.whiteSpace haskell
  string "->"
  P.whiteSpace haskell
  e <- expParser
  return $! LamE p e

apat :: Parsec String u Pat
apat = varp
  where
    varp = VarP . mkName <$> P.identifier haskell


var :: Parsec String u Exp
var = VarE . mkName <$> P.identifier haskell


lit :: Parsec String u Exp
lit = LitE <$> (charLit
            <|> stringLit
            <|> natOrFloatLit)
  where
    charLit = CharL <$> P.charLiteral haskell
    stringLit = StringL <$> P.stringLiteral haskell
    natOrFloatLit = do
        n <- P.naturalOrFloat haskell
        case n of
            Left i -> return $! IntegerL i
            Right d -> return $! RationalL (toRational d)
    -- TODO: Prim

sig :: Parsec String u Exp
sig = do
  e <- (expParser <?> "expression")
  P.whiteSpace haskell
  string "::"
  P.whiteSpace haskell
  t <- typ
  return $! SigE e t

typ :: Parsec String u Type
typ = conT
  where
    conT = do
      s <- satisfy isUpper
      r <- many (P.identLetter haskellStyle)
      return $! (ConT $ mkName $ s : r)

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.Haskell.Expression.Lexer where

import Data.Char
import Text.Parsec
import Text.Parsec.Language (haskell, haskellStyle)
import qualified Text.Parsec.Token as P
import Language.Haskell.TH.Syntax

-- TODO: Rename this file

type SParser u v = Parsec String u v

contents :: SParser u Exp -> SParser u Exp
contents p = do
  whiteSpace
  r <- p
  eof
  return r

-- Remove left recursion
expParser :: SParser u Exp
expParser = try (app <?> "function application")
        --- <|> try sig  -- left recursive
        <|> try (lit <?> "literal")
        <|> try (ifte <?> "conditional")
        <|> try (lambdaabs <?> "lambda")
        <|> try (list <?> "list")
        <|> try (var <?> "variable")
        <|> try (caseE <?> "case")
  -- TODO: A lot...


app :: SParser u Exp
app = do
  fn <- var <|> lambdaabs
  whiteSpace
  v  <- var <|> lit
  return $! AppE fn v

lambdaabs :: SParser u Exp
lambdaabs = do
  char '\\'
  whiteSpace
  p <- many1 apat
  whiteSpace
  string "->"
  whiteSpace
  e <- expParser
  return $! LamE p e

apat :: SParser u Pat
apat = varp
  where
    varp = VarP . mkName <$> identifier


var :: SParser u Exp
var = VarE . mkName <$> identifier


lit :: SParser u Exp
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


sig :: SParser u Exp
sig = do
  e <- (expParser <?> "expression")
  whiteSpace
  string "::"
  whiteSpace
  t <- typ
  return $! SigE e t

ifte :: SParser u Exp
ifte = do
  reserved "if"
  cond <- expParser
  reserved "then"
  t <- expParser
  reserved "else"
  f <- expParser
  return $! CondE cond t f

list :: SParser u Exp
list = do
  char '['
  optional whiteSpace
  exps <- sepBy expParser (optional whiteSpace >> char ',')
  optional whiteSpace
  char ']'
  return $! ListE exps

caseE :: SParser u Exp
caseE = do
  reserved "case"
  e <- expParser
  reserved "of"
  m <- many match
  return $! CaseE e m

match :: SParser u Match
match = do
  p <- apat
  reserved "->"
  b <- body
  -- TODO: where clauses
  return $! Match p b []

body :: SParser u Body
body = NormalB <$> expParser
  -- TODO: GuardedB case
  -- TODO: This TH non-terminal actually seems to correspond to two different
  -- notions in parsing. Figure out whether we should have one parser for both,
  -- or split it up.

dec :: SParser u Dec
dec = vald
  where
    vald = do
      p <- apat
      reserved "="
      b <- body
      -- TODO: where clauses
      return $! ValD p b []


typ :: SParser u Type
typ = conT
  where
    conT = do
      s <- satisfy isUpper
      r <- many identLetter
      return $! (ConT $! mkName $! s : r)

whiteSpace :: SParser u ()
whiteSpace = P.whiteSpace haskell

reserved :: String -> SParser u ()
reserved = P.reserved haskell

identifier :: SParser u String
identifier = P.identifier haskell

identLetter :: SParser u Char
identLetter = P.identLetter haskellStyle

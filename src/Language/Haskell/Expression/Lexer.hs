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


-- Remove left recursion
expParser :: SParser u Exp
expParser = try (app <?> "function application")
        --- <|> try sig  -- left recursive
        <|> try (arithSeqE <?> "list range")
        <|> try (LitE <$> lit <?> "literal")
        <|> try (ifte <?> "conditional")
        <|> try (lambdaabs <?> "lambda")
        <|> try (list <?> "list")
        <|> try (var <?> "variable")
        <|> try (caseE <?> "case")
        <|> try (letE <?> "let")
        <|> try (parensE <?> "parenthesis")
        <|> try (doE <?> "parenthesis")
  -- TODO: A lot...


app :: SParser u Exp
app = do
  fn <- var <|> lambdaabs
  whiteSpace
  v  <- var <|> (LitE <$> lit)
  return $! AppE fn v

lambdaabs :: SParser u Exp
lambdaabs = do
  char '\\'
  whiteSpace
  p <- many1 pat
  whiteSpace
  string "->"
  whiteSpace
  e <- expParser
  return $! LamE p e

pat :: SParser u Pat
pat = varp
  <|> (LitP <$> lit)
  where
    varp = VarP . mkName <$> identifier


var :: SParser u Exp
var = VarE . mkName <$> identifier


lit :: SParser u Lit
lit = charLit
   <|> stringLit
   <|> natOrFloatLit
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

letE :: SParser u Exp
letE = do
  reserved "let"
  d <- dec `sepBy` reserved ";"
  reserved "in"
  e <- expParser
  return $! LetE d e

parensE :: SParser u Exp
parensE = ParensE <$> parens expParser

doE :: SParser u Exp
doE = reserved "do" >> DoE <$> many1 stmt

arithSeqE :: SParser u Exp
arithSeqE = do
  reserved "["
  r <- range
  reserved "]"
  return $! ArithSeqE r

stmt :: SParser u Stmt
stmt = try bindS
   <|> try (NoBindS <$> expParser)
   <|> letS
  where
    letS = reserved "let" >> LetS <$> dec `sepBy1` reserved ";"
    bindS = do
      p <- pat
      reserved "<-"
      e <- expParser
      return $! BindS p e


match :: SParser u Match
match = do
  p <- pat
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
      p <- pat
      reserved "="
      b <- body
      -- TODO: where clauses
      return $! ValD p b []


typ :: SParser u Type
typ = conT
  <|> varT
  <|> listT
  -- <|> appT  -- left recursive
  where
    conT = do
      s <- satisfy isUpper
      r <- many identLetter
      return $! (ConT $! mkName $! s : r)
    varT = VarT . mkName <$> identifier
    appT = AppT <$> typ <*> typ
    listT = reserved "[" >> reserved "]" >> return ListT


range :: SParser u Range
range = try fromR
    {-<|> try fromThenR-}
    {-<|> try fromToR-}
    {-<|> try fromThenToR-}
  where
    -- TODO: accept no whitespace
    fromR = FromR <$> expParser <* reserved ".."

-- * Lexer

whiteSpace :: SParser u ()
whiteSpace = P.whiteSpace haskell

reserved :: String -> SParser u ()
reserved = P.reserved haskell

identifier :: SParser u String
identifier = P.identifier haskell

parens :: SParser u a -> SParser u a
parens = P.parens haskell

identLetter :: SParser u Char
identLetter = P.identLetter haskellStyle

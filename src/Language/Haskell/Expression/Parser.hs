{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.Haskell.Expression.Parser where

import           Control.Applicative        ((<$>), (<*), (*>))
import           Data.Char
import           Language.Haskell.TH.Syntax
import           Text.Parsec
import           Text.Parsec.Language       (haskell, haskellStyle)
import qualified Text.Parsec.Token          as P

type SParser u v = Parsec String u v


expParser :: SParser u Exp
expParser = try sig
        <|> try infixE
        <|> try expParserWInfixE

expParserWInfixE :: SParser u Exp
expParserWInfixE = try infixE
               <|> try expParserNoLR

expParserNoLR :: SParser u Exp
expParserNoLR = try (app <?> "function application")
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
  e <- expParserWInfixE
  reserved "::"
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

-- N.B.: can't handle fixity/multiple exps
infixE :: SParser u Exp
infixE = do
  f <- expParserNoLR
  op <- VarE . mkName <$> operator
  s <- expParserNoLR
  return $! UInfixE f op s

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
  char '['
  optional whiteSpace
  r <- range
  optional whiteSpace
  char ']'
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
    -- appT = AppT <$> typ <*> typ
    listT = reserved "[" >> reserved "]" >> return ListT


range :: SParser u Range
range = try fromThenToR
    <|> try fromToR
    <|> try fromThenR
    <|> try fromR
  where
    -- TODO: accept no whitespace
    fromR = FromR <$> expParser <* reserved ".."
    fromToR = do
      f <- expParser
      reserved ".."
      s <- expParser
      return $! FromToR f s
    fromThenR = do
      f <- expParser
      mwsToken $ char ','
      s <- mwsToken expParser
      string ".."
      return $! FromThenR f s
    fromThenToR = do
      f <- expParser
      reserved ","
      s <- expParser
      reserved ".."
      t <- expParser
      return $! FromThenToR f s t

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

operator :: SParser u String
operator = P.operator haskell

mwsToken :: SParser u a -> SParser u a
mwsToken x = optional whiteSpace *> x <* optional whiteSpace

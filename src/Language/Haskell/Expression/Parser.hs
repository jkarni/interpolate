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
        <|> try expParserNoLR

expParserNoLR :: SParser u Exp
expParserNoLR = try (app <?> "function application")
            <|> try (LitE <$> lit <?> "literal")
            <|> try (list <?> "list")
            <|> try (var <?> "variable")


app :: SParser u Exp
app = do
  fn <- var
  whiteSpace
  v  <- expParser
  return $! AppE fn v

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

list :: SParser u Exp
list = do
  char '['
  optional whiteSpace
  exps <- commaSep expParser
  optional whiteSpace
  char ']'
  return $! ListE exps

sig :: SParser u Exp
sig = do
  e <- expParserNoLR
  reserved "::"
  t <- typ
  return $! SigE e t

typ :: SParser u Type
typ = conT
  <|> varT
  <|> listT
  where
    conT = do
      s <- satisfy isUpper
      r <- many identLetter
      return $! (ConT $! mkName $! s : r)
    varT = VarT . mkName <$> identifier
    listT = reserved "[" >> reserved "]" >> return ListT


-- * Lexer

whiteSpace :: SParser u ()
whiteSpace = P.whiteSpace haskell

reserved :: String -> SParser u ()
reserved = P.reserved haskell

identifier :: SParser u String
identifier = P.identifier haskell

identLetter :: SParser u Char
identLetter = P.identLetter haskellStyle

commaSep :: SParser u a -> SParser u [a]
commaSep = P.commaSep haskell

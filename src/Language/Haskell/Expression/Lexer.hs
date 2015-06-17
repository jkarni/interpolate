{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Expression.Lexer where

import Data.List
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (haskell)
import qualified Text.Parsec.Token as P
import Language.Haskell.TH.Syntax

contents p = do
  P.whiteSpace haskell
  r <- p
  eof
  return r

expParser :: Parsec String u Exp
expParser = infixexp <?> "infixexp"

infixexp :: Parsec String u Exp
infixexp = (lexp <?> "lexp")
       <|> (fexp <?> "fexp")

lexp :: Parsec String u Exp
lexp = lambdaabs <?> "lambda"
   -- <|> letexp

fexp :: Parsec String u Exp
fexp = do
    mfn <- optionMaybe fexp
    v   <- aexp
    return $! case mfn of
        Just fn -> AppE fn  v
        Nothing -> v

aexp :: Parsec String u Exp
aexp = qvar

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

qvar :: Parsec String u Exp
qvar = do
    f <- P.identifier haskell
    char '.'
    s <- P.identifier haskell `sepBy1` char '.'
    return . VarE $ mkName (intercalate "." (f:s))


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


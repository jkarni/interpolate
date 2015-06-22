module Language.Haskell.ExpressionSpec (spec) where

import           Test.Hspec

import           Language.Haskell.Expression
import           Language.Haskell.Expression.Lexer

import qualified Language.Haskell.Meta.Parse as M
import           Language.Haskell.TH.Syntax



spec :: Spec
spec = do

  describe "parseExpression" $ do

    context "variables" $ do
      it "parses variables" $ testEqExp "foo"

    context "literal" $ do
      it "parses Char literals" $ testEqExp "'c'"
      it "parses String literals" $ testEqExp "\"str\""
      it "parses Rational literals" $ testEqExp "5"
      it "parses float literals" $ testEqExp "2.5"

    context "application" $ do
      it "parses function application" $ testEqExp "f x"

    context "lambdas" $ do
      it "parses single-arg lambdas" $ testEqExp "\\ x -> x"

    context "if-then-else" $ do
      it "parses it" $ testEqExp "if cond then x else y"

    context "list" $ do
      it "parses empty lists" $ testEqExp "[]"
      it "parses non-empty lists" $ testEqExp "[1,2]"

    context "list ranges" $ do
      it "parses simple 'from' ranges" $ do
        testEqExp "[ 1 .. ]"
        {-testEqExp "[1..]"-}
      it "parses 'from-then' ranges" $ do
        testEqExp "[ 1, 2 .. ]"
        {-testEqExp "[1,2..]"-}
      it "parses 'from-to' ranges" $ do
        testEqExp "[ 1 .. 2 ]"
        {-testEqExp "[1..2]"-}
      it "parses 'from-then-to' ranges" $ do
        testEqExp "[ 1 , 2 .. 10 ]"
        {-testEqExp "[1, 2..10]"-}

    context "case statements" $ do
      it "parses a single pattern" $ testEqExp "case e of y -> 5"
      it "parses multiple patterns" $ pendingWith "layout and curly braces"

    context "let bindings" $ do
      it "parses them" $ testEqExp "let x = 5 in x"
      it "parses multiple assignments separated by ';'" $
        testEqExp "let x = 5 ; f = id in f x"
      it "parses multiple assignments separated by newlines" $
        pendingWith "layout and curly braces"

    context "parens" $ do
      it "parses them with spaces" $ testEqExp "( x )"
      it "parses them without spaces" $ testEqExp "(x)"

    context "type signatures" $ do
      it "accepts variables with type signatures" $ pendingWith "left recursive"
        --testEqExp "foo :: Int"

    context "do notation" $ do
      it "accepts single-line do" $ testEqExp "do return 5"
      -- it "accepts multiple do statements" $ testEqExp "do { print 2 ; return 5}"


  describe "parseType" $ do

    context "types" $ do
      it "parses single types" $ testEqType "Int"
      it "parses single type variables" $ testEqType "a"
      {-it "parses applied higher-kinded types" $ testEqType "IO Int"-}
      {-it "parses list types" $ do-}
        {-testEqType "[] Int"-}
        {-testEqType "[Int]"-}
      {-it "parses tuples" $ testEqType "(Int, String)"-}
      {-it "parses unit" $ testEqType "()"-}

  describe "parsePat" $ do

    it "parses variable patterns" $ testEqPat "x"
    it "parses literal patterns" $ testEqPat "5"


testEqExp :: String -> Expectation
testEqExp e = parseExpression e `shouldBe` M.parseExp e

testEqType :: String -> Expectation
testEqType e = parseType e `shouldBe` M.parseType e

testEqPat :: String -> Expectation
testEqPat e = parsePat e `shouldBe` M.parsePat e

module Language.Haskell.ExpressionSpec (spec) where

import           Test.Hspec

import           Language.Haskell.Expression

import           Language.Haskell.Meta.Parse.Careful (parseExp)
import           Language.Haskell.TH.Syntax



spec :: Spec
spec = do
  let testEq e = parseExpression e `shouldBe` parseExp e

  describe "parseExpression" $ do

    context "variables" $ do
      it "parses variables" $ testEq "foo"

    context "literal" $ do
      it "parses Char literals" $ testEq "'c'"
      it "parses String literals" $ testEq "\"str\""
      it "parses Rational literals" $ testEq "5"
      it "parses float literals" $ testEq "2.5"

    context "application" $ do
      it "parses function application" $ testEq "f x"

    context "lambdas" $ do
      it "parses single-arg lambdas" $ testEq "\\ x -> x"

    context "if-then-else" $ do
      it "parses it" $ testEq "if cond then x else y"

    context "list" $ do
      it "parses empty lists" $ testEq "[]"
      it "parses non-empty lists" $ testEq "[1,2]"
      it "parses list ranges" $ do
        testEq "[ 1 .. ]"
        --testEq "[ 1..2 ]"
        --testEq "[1 .. 2]"
        --testEq "[1, 2..10]"

    context "case statements" $ do
      it "parses a single pattern" $ testEq "case e of y -> 5"
      it "parses multiple patterns" $ pendingWith "layout and curly braces"

    context "let bindings" $ do
      it "parses them" $ testEq "let x = 5 in x"
      it "parses multiple assignments separated by ';'" $
        testEq "let x = 5 ; f = id in f x"
      it "parses multiple assignments separated by newlines" $
        pendingWith "layout and curly braces"

    context "parens" $ do
      it "parses them with spaces" $ testEq "( x )"
      it "parses them without spaces" $ testEq "(x)"

    context "type signatures" $ do
      it "accepts variables with type signatures" $ pendingWith "left recursive"
        --testEq "foo :: Int"

    context "do notation" $ do
      it "accepts single-line do" $ testEq "do return 5"
      -- it "accepts multiple do statements" $ testEq "do { print 2 ; return 5}"

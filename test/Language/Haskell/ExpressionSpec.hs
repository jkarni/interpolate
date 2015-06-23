module Language.Haskell.ExpressionSpec (spec) where

import           Test.Hspec
import qualified Language.Haskell.Meta.Parse as M

import           Language.Haskell.Expression


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
      it "parses function application over complex expressions" $ testEqExp "f [x,y]"

    context "list" $ do
      it "parses empty lists" $ testEqExp "[]"
      it "parses non-empty lists" $ testEqExp "[1,2]"
      it "accepts optional whitespace" $ do
        testEqExp "[1, 2]"
        testEqExp "[ 1, 2 ]"
        testEqExp "[ 1 , 2 ]"

    context "type signatures" $ do
      it "accepts variables with type signatures" $ testEqExp "foo :: Int"


testEqExp :: String -> Expectation
testEqExp e = parseExp e `shouldBe` M.parseExp e

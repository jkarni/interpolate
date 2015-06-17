module Language.Haskell.ExpressionSpec (spec) where

import           Test.Hspec

import           Language.Haskell.Expression

import           Language.Haskell.Meta.Parse.Careful (parseExp)
import           Language.Haskell.TH.Syntax



spec :: Spec
spec = do
  let testEq e = parseExpression e `shouldBe` parseExp e

  describe "parseExpression" $ do

    {-context "variables" $ do-}
      {-it "parses variables" $ testEq "foo"-}

    {-context "literal" $ do-}
      {-it "parses Char literals" $ testEq "'c'"-}
      {-it "parses String literals" $ testEq "\"str\""-}
      {-it "parses Rational literals" $ testEq "5"-}

    {-context "application" $ do-}
      {-it "parses function application" $ testEq "f x"-}

    context "lambdas" $ do
      it "parses single-arg lambdas" $ testEq "\\ x -> x"

    {-it "accepts type signatures" $ do-}
      {-let e = "foo :: Int"-}
      {-parseExpression e `shouldBe` parseExp e-}
      -- (Right $ SigE (VarE $ mkName "foo") (ConT $ mkName "Int")) `shouldBe` parseExp e

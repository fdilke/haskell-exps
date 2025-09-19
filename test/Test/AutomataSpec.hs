module Test.AutomataSpec where

import LangFeature.Automata
import Test.Hspec

spec :: Spec
spec = do
    describe "lambdas can" $ do
        it "mimic (+1) function" $
            doNothing 4 `shouldBe` 4


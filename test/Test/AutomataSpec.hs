{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module Test.AutomataSpec where

import LangFeature.Automata
import Test.Hspec

spec :: Spec
spec = do
  describe "function does nothing" $ do
    it "mimic (+1) function" $
      doNothing 4 `shouldBe` 4
  describe "george's test" $ do
    it "does stuff" $
      take 2 (listStates automaton) `shouldBe` [A,B]

data S = A | B | C deriving (Eq, Show)
automaton :: Automaton S
automaton = Automaton{initial = A, update = \_ -> B}
-- automaton =  (A,  \_ -> B)

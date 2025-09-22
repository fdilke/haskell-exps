{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Test.AutomataSpec where

import LangFeature.Automata
import Test.Hspec

spec :: Spec
spec = do
  describe "function does nothing" $ do
    it "mimic (+1) function" $
      doNothing 4 `shouldBe` 4
  describe "george's test 1" $ do
    it "does stuff" $
      take 2 (listStates1 automaton1) `shouldBe` [A,B]
  describe "george's test 2" $ do
    it "does stuff" $ do
      take 5 (listStates2 automaton2 ([4,7,12] ++ repeat 1) ) `shouldBe` [0,4,11,23,24]
      listStates2 automaton2 [4,7,12] `shouldBe`  [0,4,11,23]

data S = A | B | C deriving (Eq, Show)
automaton1 :: Automaton1 S
automaton1 = Automaton1{initial = A, update = \_ -> B}

automaton2 :: Automaton2 Int Int
automaton2 = Automaton2{initial = 0, update = \s n -> s + n}

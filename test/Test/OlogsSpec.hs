{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.OlogsSpec where

import LangFeature.Ologs
import Test.Hspec

spec :: Spec
spec = do
  describe "function successfully does very little" $ do
    it "mimic (+1) function" $
      doNothing 4 `shouldBe` 4

  describe "create a basic olog" $ do
    it "the graph olog" $
      let 
        -- x :: Int
        -- x = 3
        graphOlog :: Olog Int
        graphOlog = Olog {
            dots = [0, 1],
            arcs = [Arc ("source", 0, 1), Arc ("target", 1, 0)],
            identities = []
          }
      in do
        [7, 8] `shouldBe` [7, 8]
        

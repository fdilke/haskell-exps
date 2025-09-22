{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.OlogSpec where

import LangFeature.Olog
import Test.Hspec

spec :: Spec
spec = do
  describe "function successfully does nothing" $ do
    it "mimic (+1) function" $
      doNothing 4 `shouldBe` 4

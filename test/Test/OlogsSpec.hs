{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.OlogsSpec where

import LangFeature.Ologs
import Test.Hspec
import Data.Either (isRight)

type MaybeOlog = Either String (Olog Int)

spec :: Spec
spec = do
  describe "olog sanity checks" $ do
    it "arcs must have a source of a known dot" $
      let
        badOlog :: MaybeOlog
        badOlog =
          makeOlog [1] [("source", 0, 1)] [] 
      in
        badOlog `shouldBe` Left "bad source: 0"
    it "arcs must have a target of a known dot" $
      let
        badOlog :: MaybeOlog
        badOlog =
          makeOlog [0] [("source", 0, 1)] []
      in
        badOlog `shouldBe` Left "bad target: 1"
    it "arc is then ok" $
      let
        goodOlog :: MaybeOlog
        goodOlog =
            makeOlog [0, 1] [("y", 1, 0), ("x", 0, 1)] []
      in
        isRight goodOlog `shouldBe` True

  -- describe "create a basic olog" $ do
  --   it "the graph olog" $
  --     let -- x :: Int
  --         -- x = 3
  --         graphOlog :: Either String (Olog Int)
  --         graphOlog =
  --           makeOlog [0, 1] [("source", 0, 1), ("target", 1, 0)] []
  --     in do
  --           [7, 8] `shouldBe` [7, 8]

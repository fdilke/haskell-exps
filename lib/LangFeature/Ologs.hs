{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Collapse lambdas" #-}

module LangFeature.Ologs where

-- import GHC.StaticPtr (staticPtrKeys)

doNothing :: Int -> Int
doNothing x = x

newtype Arc dot = Arc (String, dot, dot)
data Identity = Identity {
    lhs :: [String],
    rhs :: [String]
}

data Olog dot = Olog
  { 
    dots :: [dot],
    arcs :: [Arc dot],
    identities :: [Identity]
  }


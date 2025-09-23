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

data Arc dot = Arc {
  arcId :: String,
  source :: dot,
  target :: dot
  }
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

sanity :: forall dot. Eq dot => Olog dot -> Bool
sanity olog = all (\arc -> arc.source `elem` olog.dots && arc.target `elem` olog.dots) olog.arcs

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module LangFeature.Ologs
  ( Arc,
    Identity,
    Olog,
    makeOlog,
  )
where

import Data.Maybe

data Arc dot = Arc
  { name :: String,
    source :: dot,
    target :: dot
  } deriving (Show, Eq)

data Identity = Identity
  { lhs :: [String],
    rhs :: [String]
  } deriving (Show, Eq)

data Olog dot = Olog
  { dots :: [dot],
    arcs :: [Arc dot],
    identities :: [Identity]
  } deriving (Show, Eq)

makeOlog :: (Eq dot, Show dot) => [dot] -> [(String, dot, dot)] -> [Identity] -> Either String (Olog dot)
makeOlog dots arcs identities = do
  -- let sourceChecks = map (\arc -> arc.source `elem` dots && arc.target `elem` dots) arcs
  -- let sourceChecks = map (\arc -> arc.source `elem` dots) arcs
  -- let sourceErrors = catMaybes $ map (\source -> if source `elem` dots then Nothing else Just source) $ map (\arc -> arc.source ) arcs
  -- let sourceErrors = mapMaybe (\source -> if source `elem` dots then Nothing else Just source) (map (\arc -> arc.source ) arcs)
  let sources = map (\(_,src,_) -> src) arcs
  let targets = map (\(_,_,tgt) -> tgt) arcs
  let inDots source = if source `elem` dots then Nothing else Just source
  let sourceErrors = mapMaybe inDots sources
  let targetErrors = mapMaybe inDots targets
  let mapArcs = map \(name, src, tgt) -> Arc { name=name, source=src, target=tgt }
  case sourceErrors of
      [] -> case targetErrors of
        [] -> Right $ Olog dots (mapArcs arcs) identities
        e : _ -> Left $ "bad target: " <> show e
      e : _ -> Left $ "bad source: " <> show e
  -- sourceFail match 
  --   Just failingO => Left helpfulMessage
  --   Nothing =>
  --   targetFail match 
  --   Just failingO => Left helpfulMessage
  --     Nothing =>
  --    Right olof
  -- if sanity
  --   then _
  --   else Left _

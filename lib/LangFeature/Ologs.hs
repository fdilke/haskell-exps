{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move catMaybes" #-}
{-# HLINT ignore "Use mapMaybe" #-}
{-# HLINT ignore "Use list comprehension" #-}

module LangFeature.Ologs
  ( Arc,
    Identity,
    Olog,
    makeOlog,
  )
where

import Control.Monad
import Data.Maybe
import Data.Traversable
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map

data Arc dot = Arc
  { name :: String,
    source :: dot,
    target :: dot
  }
  deriving (Show, Eq)

data Identity = Identity
  { lhs :: [String],
    rhs :: [String]
  }
  deriving (Show, Eq)

data Olog dot = Olog
  { dots :: [dot],
    arcs :: [Arc dot],
    identities :: [Identity]
  }
  deriving (Show, Eq)

makeOlog :: forall dot. (Eq dot, Show dot) => [dot] -> [(String, dot, dot)] -> [([String], [String])] -> Either String (Olog dot)
makeOlog dots preArcs preIdentities =
  case errors of
    [] ->
      Right $
        Olog
          dots
          (map (\(name, src, tgt) -> Arc {name = name, source = src, target = tgt}) preArcs)
          ( (\(path1, path2) -> Identity {lhs = path1, rhs = path2}) <$> preIdentities
          )
    err : _ -> Left err
  where
    errors = arcErrors <> identityErrors
    arcErrors =
      concat . concat $
        map
          ( fmap maybeToList . \(dotMapper, errorPrefix) ->
              map ((\dot -> if dot `elem` dots then Nothing else Just $ errorPrefix <> show dot) . dotMapper) preArcs
          )
          [ (\(_, src, _) -> src, "bad source: "),
            (\(_, _, tgt) -> tgt, "bad target: ")
          ]
    knownArcNames = map (\(name,_,_) -> name) preArcs
    identityErrors = identityKnownArcErrors <> identityLhsJoinErrors <> identityRhsJoinErrors
    identityKnownArcErrors =
      concat $
        map
          ( \(lhs, rhs) ->
              (if null lhs && null rhs then ["forbidden trivial identity"] else [])
               <>
              catMaybes
              (map
                ( \arcName ->
                    if arcName `elem` knownArcNames
                      then Nothing
                      else Just $ "bad arc: " <> arcName
                )
                $ lhs <> rhs)
          )
          preIdentities
    namesToArcs :: Map String (dot, dot) = _
    identityLhsJoinErrors = identityXhsJoinErrors (\(lhs, _) -> lhs)
    identityRhsJoinErrors = identityXhsJoinErrors (\(_, rhs) -> rhs)
    identityXhsJoinErrors :: (([String], [String]) -> [String]) -> [String]
    identityXhsJoinErrors picker = [] where
      arcNames :: [[String]] = picker <$> preIdentities
    checkTerm :: [String] -> Maybe String
    checkTerm arcNames =
      if targets == sources then Nothing else
          Just $ "bad identity xhs:" <> show arcNames
      where
        arcs :: [(dot, dot)] = catMaybes $ flip Map.lookup namesToArcs <$> arcNames
        targets :: [dot] = tail $ snd <$> arcs
        sources :: [dot] = init $ fst <$> arcs
        


data MakeOlogError dot
  = BadSource String dot
  | BadTarget String dot
  | ForbiddenTrivialIdentity
  | BadArc String
  deriving (Show)

makeOlog' :: (Eq dot) => [dot] -> [(String, dot, dot)] -> [([String], [String])] -> Either (MakeOlogError dot) (Olog dot)
makeOlog' dots preArcs preIdentities = do
  arcs <- for preArcs \(name, source, target) -> do
    -- if source `elem` dots then pure () else Left $ BadSource name source
    unless (source `elem` dots) $ Left $ BadSource name source
    unless (target `elem` dots) $ Left $ BadTarget name target
    pure Arc {name, source, target}
  identities <- for preIdentities \(lhs, rhs) -> do
    when (null lhs && null rhs) $ Left ForbiddenTrivialIdentity
    for_ (lhs <> rhs) \arc -> unless (arc `elem` knownArcNames) $ Left $ BadArc arc
    pure Identity {lhs, rhs}
  pure Olog {dots, arcs, identities}
  where
    knownArcNames = map (\(name,_,_) -> name) preArcs

-- unless' b x = if b then pure () else x
-- when' b x = if b then x else pure ()

bind = (>>=)

-- checkArc :: (PreArc dot -> dot, String) -> PreArc dot -> Maybe String
-- checkArc (mapper, errorPrefix) preArc =
--   if mapper preArc `elem` dots then Nothing else Just $ errorPrefix <> show dot
-- checkers :: [(PreArc dot -> dot, String)] =
--   [ (\(_, src, _) -> src, "bad source: "),
--     (\(_, _, tgt) -> tgt, "bad target: ")
--   ]
-- applyChecker :: (PreArc dot -> dot, String) -> Maybe String
-- applyChecker (mapper, prefix) =
--   map mapper arcs
-- rawStrings :: [Maybe String] = map applyChecker [
--   (\(_, src, _) -> src, "bad source: "),
--     (\(_, _, tgt) -> tgt, "bad target: ")
--   ]
-- errors = []

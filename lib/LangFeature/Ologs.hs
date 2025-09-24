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
{-# LANGUAGE TypeOperators #-}

module LangFeature.Ologs
  ( Arc,
    Identity,
    Olog,
    makeOlog,
    MakeOlogError(..),
  )
where

import Control.Monad
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable

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

makeOlog ::
  forall dot.
  (Eq dot, Show dot) =>
  [dot] ->
  [(String, dot, dot)] ->
  [([String], [String])] ->
  Either (MakeOlogError dot) (Olog dot)
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
    errorUnless b e = if b then Nothing else Just e
    errors :: [MakeOlogError dot] = arcErrors <> identityErrors
    arcErrors =
      concat . concat $
        map
          ( fmap maybeToList . \(dotMapper, errorPrefix) ->
              map (\arc@(name, _, _) -> (\dot -> errorUnless (dot `elem` dots) $ errorPrefix name dot) $ dotMapper arc) preArcs
          )
          [ (\(_, src, _) -> src, UnknownSource),
            (\(_, _, tgt) -> tgt, UnknownTarget)
          ]
    knownArcNames = map (\(name, _, _) -> name) preArcs
    identityErrors :: [MakeOlogError dot] = 
      identityKnownArcErrors <> identityLhsJoinErrors <> identityRhsJoinErrors <> identityMismatchErrors
    identityKnownArcErrors =
      concat $
        map
          ( \(lhs, rhs) ->
              (if null lhs && null rhs then [ForbiddenTrivialIdentity] else [])
                <> catMaybes
                  ( map
                      (\arcName -> errorUnless (arcName `elem` knownArcNames) $ UnknownArc arcName)
                      $ lhs <> rhs
                  )
          )
          preIdentities
    namesToArcs :: Map String (dot, dot) = Map.fromList $ (\(s, src, tgt) -> (s, (src, tgt))) <$> preArcs
    identityLhsJoinErrors = identityXhsJoinErrors BadIdentityLhs fst
    identityRhsJoinErrors = identityXhsJoinErrors BadIdentityRhs snd
    identityXhsJoinErrors :: ([String] -> MakeOlogError dot) -> (([String], [String]) -> [String]) ->
      [MakeOlogError dot]
    identityXhsJoinErrors errorFactory picker = catMaybes $ map (checkTerm errorFactory . picker) preIdentities
    checkTerm :: ([String] -> MakeOlogError dot) -> [String] ->  Maybe (MakeOlogError dot)
    checkTerm errorFactory arcNames = errorUnless (targets == sources) $ errorFactory arcNames
      where
        arcs :: [(dot, dot)] = catMaybes $ flip Map.lookup namesToArcs <$> arcNames
        targets :: [dot] = tail $ snd <$> arcs
        sources :: [dot] = init $ fst <$> arcs
    identityMismatchErrors = catMaybes $ checkMismatch <$> preIdentities
    checkMismatch :: ([String], [String]) -> Maybe (MakeOlogError dot)
    checkMismatch (lhs, rhs) = 
      case (leftSignature, rightSignature) of
        (_, Nothing) -> Nothing
        (Nothing, _) -> Nothing
      errorUnless (leftSignature == rightSignature) $ IdentityMismatch lhs rhs leftSignature rightSignature
      where
        leftSignature = signature lhs
        rightSignature = signature rhs
        signature :: [String] -> Maybe (dot, dot)
        signature terms =
          case (Map.lookup (head terms) namesToArcs) of
            Nothing -> Nothing
            Just (src, _) ->
              case (Map.lookup (last terms) namesToArcs) of
                Nothing -> Nothing
                Just (_, tgt) -> Just (src, tgt)


-- type f $ x = f x
-- type ($) f x = f x


data MakeOlogError dot
  = UnknownSource String dot
  | UnknownTarget String dot
  | ForbiddenTrivialIdentity
  | UnknownArc String
  | BadIdentityLhs [String]
  | BadIdentityRhs [String]
  | IdentityMismatch [String] [String] (dot, dot) (dot, dot)
  deriving (Show, Eq)

makeOlog' :: forall dot. (Eq dot) => [dot] -> [(String, dot, dot)] -> [([String], [String])] -> Either (MakeOlogError dot) (Olog dot)
makeOlog' dots preArcs preIdentities = do
  arcs <- for preArcs \(name, source, target) -> do
    unless (source `elem` dots) $ Left $ UnknownSource name source
    unless (target `elem` dots) $ Left $ UnknownTarget name target
    pure Arc {name, source, target}
  identities <- for preIdentities \(lhs, rhs) -> do
    when (null lhs && null rhs) $ Left ForbiddenTrivialIdentity
    -- for_ (lhs <> rhs) \arc -> unless (arc `elem` knownArcNames) $ Left $ UnknownArc arc
    for_ (lhs <> rhs) \arc -> do
      case Map.lookup arc namesToArcs of
        Just (source, target) -> do
          pure ()
        Nothing -> Left $ UnknownArc arc
    -- unless (arc `elem` map (\(name,_,_) -> name) preArcs) $ Left $ UnknownArc arc
    pure Identity {lhs, rhs}
  pure Olog {dots, arcs, identities}
  where
    namesToArcs :: Map String (dot, dot)
    -- namesToArcs = map (\(name,_,_) -> name) preArcs
    namesToArcs = Map.fromList $ (\(s, src, tgt) -> (s, (src, tgt))) <$> preArcs
    pairAdjacent xs = zip xs $ tail xs

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

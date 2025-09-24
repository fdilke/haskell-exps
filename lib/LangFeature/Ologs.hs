{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module LangFeature.Ologs
  ( Arc,
    Identity,
    Olog,
    makeOlog,
  )
where

import Data.Maybe
import Control.Monad
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

makeOlog :: forall dot. (Eq dot, Show dot) => [dot] -> [(String, dot, dot)] -> [Identity] -> Either String (Olog dot)
makeOlog dots arcs identities =
  case errors of
    [] -> Right $ Olog dots (map (\(name, src, tgt) -> Arc {name = name, source = src, target = tgt}) arcs) identities
    err : _ -> Left err
  where
    errors =
        concat . concat
            -- . map (fmap maybeToList)
            $ map
                ( fmap maybeToList . \(dotMapper, errorPrefix) ->
                    map ((\dot -> if dot `elem` dots then Nothing else Just $ errorPrefix <> show dot) . dotMapper) arcs
                )
                [ (\(_, src, _) -> src, "bad source: ")
                , (\(_, _, tgt) -> tgt, "bad target: ")
                ]


data MakeOlogError dot
    = BadSource String dot
    | BadTarget String dot
    deriving (Show)
makeOlog' :: (Eq dot) => [dot] -> [(String, dot, dot)] -> [Identity] -> Either (MakeOlogError dot) (Olog dot)
makeOlog' dots arcs0 identities = do
    arcs <- for arcs0 \(name, source, target) -> do
      -- unless (source `elem` dots) $ Left $ BadSource name source
      if source `elem` dots then pure () else Left $ BadSource name source
      unless (target `elem` dots) $ Left $ BadTarget name target
      pure Arc{name, source, target}
    pure Olog{dots, arcs, identities}
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

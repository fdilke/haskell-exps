{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE TypeApplications #-}

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
      concat @[] @String $
        concat @Maybe @[String] $
        -- (concat :: (Maybe [[String]] -> [[String]])) $
          traverse
            ( sequence . \(dotMapper, errorPrefix) ->
                map ((\dot -> if dot `elem` dots then Nothing else Just $ errorPrefix <> show dot) . dotMapper) arcs
            )
            [ (\(_, src, _) -> src, "bad source: "),
              (\(_, _, tgt) -> tgt, "bad target: ")
            ]

module Main (main) where
import Test.Hspec

import qualified Test.FastLaneSpec as FLS
import qualified Test.FooSpec as Foo
import qualified Test.KickTyresSpec as KickTyres
import qualified Test.NewtonSpec as Newton
import qualified Test.RecordTypesSpec as RecordTypes
import qualified Test.AutomataSpec as Automata
import qualified Test.OlogSpec as Olog
--import Test.FooSpec

main :: IO ()
main = hspec $ do
    describe "Fast Lane" FLS.spec
    describe "Foo" Foo.spec
    describe "KickTyres" KickTyres.spec
    describe "Newton" Newton.spec
    describe "RecordTypes" RecordTypes.spec
    describe "Automata" Automata.spec
    describe "Olog" Olog.spec
--    describe "My amazing tests" [ FastLaneSpec FooSpec ]


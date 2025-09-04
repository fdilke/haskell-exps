module Main (main) where
import Test.Hspec

--import Test.FastLaneSpec
--import Test.FooSpec

main :: IO ()
main = hspec $ do
    describe "My amazing tests"  $ pure ()
--    describe "My amazing tests" [ FastLaneSpec FooSpec ]


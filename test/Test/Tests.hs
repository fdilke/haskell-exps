module Main where

import Test.DocTest

main :: IO ()
main = do
    putStrLn("Running the tests at least")
    doctest ["LangFeature/KickTyres.hs"]
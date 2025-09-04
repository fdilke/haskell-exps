module LangFeature.Newton(mySqrt) where

import Data.List(find)

newt :: Fractional a => a -> a -> a
newt q x = (x + q/x)/2

closeEnough :: Ord a => Fractional a => a -> a -> a -> Bool
closeEnough q tolerance est = (abs (est * est - q)) < tolerance

standardTolerance :: Fractional a => a
standardTolerance = 1e-6

mySqrt :: Ord a => Fractional a => a -> a
mySqrt q = case (find (closeEnough q standardTolerance) (iterate (newt q) (q/2))) of
    Just root -> root
    Nothing -> -1


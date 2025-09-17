{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LangFeature.Newton(mySqrt) where

import Data.List(find)

-- newt :: Fractional a => a -> a -> a
-- newt q x = (x + q/x)/2

closeEnough :: Ord a => Fractional a => a -> a -> a -> Bool
closeEnough q tolerance est = abs (est * est - q) < tolerance

standardTolerance :: Fractional a => a
standardTolerance = 1e-6

myNewton :: Ord a => Fractional a => (a -> a) -> (a -> a) -> a -> a
myNewton f f' x =
  x - f x / f' x

mySqrt :: forall a. Ord a => Fractional a => a -> a
mySqrt q =
   case find (closeEnough q standardTolerance) (iterate newt (q/2)) of
     Just root -> root
     Nothing -> -1
    where
      z :: a -> a -> a
      z p x = x*x - p
      z' :: a -> a -> a
      z' _ x = 2 * x
      f :: a -> a
      f x = x*x - q
      f' :: a -> a
      f' x = x*x - q
      newt :: a -> a
       -- newt p x = (x + p/x)/2
      newt x = x - f x/f' x

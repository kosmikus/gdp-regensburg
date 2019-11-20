{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
module GDPSort
  (sortBy', mergeBy', SortedBy())
  where

import Data.Coerce
import Data.List
import GDPMachinery

sortBy' ::
     ((a -> a -> Ordering) ~~ name)
  -> [a]
  -> SortedBy name a
sortBy' cmp xs =
  coerce (sortBy (the cmp) xs)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp [] ys = ys
mergeBy cmp xs [] = xs
mergeBy cmp (x {- ignore this dot -} : xs) (y : ys) =
  case cmp x y of
    GT -> y : mergeBy cmp (x : xs) ys
    _  -> x : mergeBy cmp xs (y : ys)

newtype SortedBy name a = SortedBy [a]

instance The (SortedBy name a) [a]

mergeBy' ::
     ((a -> a -> Ordering) ~~ name)
  -> SortedBy name a
  -> SortedBy name a
  -> SortedBy name a
mergeBy' cmp xs ys =
  coerce (mergeBy (the cmp) (the xs) (the ys))




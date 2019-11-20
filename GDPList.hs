{-# LANGUAGE TypeOperators #-}
module GDPList where

import Data.Coerce
import GDPMachinery

data IsNil xs
data IsCons xs

head' :: ([a] ~~ xs ::: IsCons xs) -> a
head' xs = head (the (the xs))

data ListCase a xs =
    IsNil  (Proof (IsNil xs))
  | IsCons (Proof (IsCons xs))

classify :: ([a] ~~ xs) -> ListCase a xs
classify xs =
  case the xs of
    []      -> IsNil axiom
    (_ : _) -> IsCons axiom

newtype Rev xs = Rev Defn

reverse' :: ([a] ~~ xs) -> ([a] ~~ Rev xs)
reverse' xs =
  defn (reverse (the xs))

lemma :: Proof (IsCons xs) -> Proof (IsCons (Rev xs))
lemma _ = axiom

example :: ([a] ~~ xs ::: IsCons xs) -> (a, a)
example xs =
  (head' xs, head' (reverse' (the xs) ... lemma (proof xs)))

-- lemma' :: Proof (Rev (Rev xs)) -> Proof xs

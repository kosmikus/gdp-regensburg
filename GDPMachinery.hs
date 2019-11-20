{-# LANGUAGE TypeOperators, RankNTypes, DefaultSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module GDPMachinery
  (name, type (~~)(), The(..))
  where

import Data.Coerce

newtype a ~~ name = Named a
infix 0 ~~

-- class Coercible a b where
--   coerce :: a -> b
--
-- instance Coercible a (a ~~ name)
-- instance Coercible (a ~~ name) a

testCoerce1 :: a -> (a ~~ name)
testCoerce1 = coerce

name :: a -> (forall name . (a ~~ name) -> r) -> r
name a k = k (coerce a)

class The a b | a -> b where
  the :: a -> b
  default the :: Coercible a b => a -> b
  the = coerce

instance The (a ~~ name) a

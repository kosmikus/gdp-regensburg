{-# LANGUAGE TypeOperators, RankNTypes, DefaultSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module GDPMachinery
  (name, type (~~)(), The(..), Proof(), axiom, defn, Defn(), Defining, (:::)(), (...), proof)
  where

import Data.Coerce

newtype a ~~ name = Named a
-- infix 0 ~~

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

data Defn = Defn

class (Coercible f Defn, Coercible Defn f) => Defining f
instance (Coercible f Defn, Coercible Defn f) => Defining f

defn :: Defining f => a -> (a ~~ f)
defn = coerce

data Proof a = QED

axiom :: Proof a
axiom = QED

newtype (:::) a p = SuchThat a

instance The (a ::: p) a

(...) :: a -> Proof p -> a ::: p
a ... _ = coerce a

proof :: (a ::: p) -> Proof p
proof _ = axiom

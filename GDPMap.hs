{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}
module GDPMap where

import Data.Coerce
import qualified Data.Map as M
import Data.Maybe
import GDPMachinery
import Prelude hiding (lookup)

newtype Map ks k v = Map (M.Map k v)
newtype Key ks k = Key k

instance The (Map ks k v) (M.Map k v)
instance The (Key ks k) k

member :: Ord k => k -> Map ks k v -> Maybe (Key ks k)
member k m =
  if M.member k (the m)
    then Just (coerce k)
    else Nothing

lookup :: Ord k => Key ks k -> Map ks k v -> v
lookup k m =
  fromJust (M.lookup (the k) (the m))

withMap :: M.Map k v -> (forall ks . Map ks k v -> r) -> r
withMap m k = k (coerce m)

map' :: (v -> v') -> Map ks k v -> Map ks k v'
map' f m =
  coerce (M.map f (the m))

modify :: Ord k => Key ks k -> v -> Map ks k v -> Map ks k v
modify k x m =
  coerce (M.insert (the k) x (the m) )

inserting ::
     Ord k => k -> v -> Map ks k v
  -> (forall ks' .
        Map ks' k v
     -> (Key ks k -> Key ks' k)
     -> Key ks' k
     -> r
     )
  -> r
inserting key val m kont =
  let
    m' = M.insert key val (the m)
  in
    kont (coerce m') coerce (coerce key)

example :: Int
example =
  let
    m = M.fromList [(1,4), (3,9)]
  in
    withMap m $ \ m' ->
      case member 1 m' of
        Just k1 ->
          inserting 5 32 m' $ \ m'' kf _ ->
            lookup (kf k1) m''
        Nothing  -> 99


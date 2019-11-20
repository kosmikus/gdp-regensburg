{-# LANGUAGE TypeApplications #-}
module GDPUse where

import GDPMachinery
import GDPRegensburg

test xs ys =
  name (flip compare) $ \ cmp ->
    let
      xs' = sortBy' cmp xs
      ys' = sortBy' cmp ys
    in
      the (mergeBy' cmp xs' ys')

{-
test' xs ys =
  name compare $ \ cmp ->
  name compare $ \ fcmp ->
    let
      xs' = sortBy'  cmp xs
      ys' = sortBy' fcmp ys
    in
      the (mergeBy' cmp xs' ys')
-}

module Yacad.ImplicitFn where

import Graphics.Implicit
import Graphics.Implicit.Primitives
import Graphics.Implicit.Definitions

apply :: (ℝ2 -> ℝ -> ℝ) -> SymbolicObj2 -> SymbolicObj2
apply fn obj = implicit fn' box
  where
    box = getBox obj
    obj_fn = getImplicit obj
    fn' p = fn p (obj_fn p)

intersect :: (ℝ2 -> ℝ) -> SymbolicObj2 -> SymbolicObj2
intersect fn = apply (\p -> max (fn p))

apply3 :: (ℝ3 -> ℝ -> ℝ) -> SymbolicObj3 -> SymbolicObj3
apply3 fn obj = implicit fn' box
  where
    box = getBox obj
    obj_fn = getImplicit obj
    fn' p = fn p (obj_fn p)

intersect3 :: (ℝ3 -> ℝ) -> SymbolicObj3 -> SymbolicObj3
intersect3 fn = apply3 (\p -> max (fn p))

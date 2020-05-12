module Yacad.ImplicitFn where

import Graphics.Implicit
import Graphics.Implicit.Primitives
import Graphics.Implicit.Definitions

apply :: (ℝ2 -> ℝ -> ℝ) -> SymbolicObj2 -> SymbolicObj2
apply fn obj = implicit fn' box
  where
    box = getBox obj
    obj_fn = getImplicit obj
    fn' p@(x, y) = fn p (obj_fn p)

intersect :: (ℝ2 -> ℝ) -> SymbolicObj2 -> SymbolicObj2
intersect fn = apply (\p -> max (fn p))

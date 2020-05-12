module Yacad.Transpose
  ( transpose3
  , Dimension(..)
  )
  where

import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives

data Dimension = X | Y | Z

extract_dim :: Dimension -> ℝ3 -> ℝ
extract_dim X = \(x, _, _) -> x
extract_dim Y = \(_, y, _) -> y
extract_dim Z = \(_, _, z) -> z

transpose3 :: (Dimension, Dimension, Dimension) -> SymbolicObj3 -> SymbolicObj3
transpose3 (tx, ty, tz) = \obj ->
  let
    (p1, p2) = getBox obj
    implic = getImplicit obj
  in
    implicit (implic . trans) (trans p1, trans p2)
  where
    trans p = (getx p, gety p, getz p)
    getx = extract_dim tx
    gety = extract_dim ty
    getz = extract_dim tz

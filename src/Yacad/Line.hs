module Yacad.Line where

import Graphics.Implicit
import Graphics.Implicit.Primitives
import Graphics.Implicit.Definitions

import Yacad.ImplicitFn (intersect)

data Line = Line {slope :: ℝ, intercept :: ℝ, transpose :: Bool}

implicit_eqn :: Line -> ℝ3
implicit_eqn (Line k q transpose)
  | transpose = (1, -k, -q)
  | otherwise = (-k, 1, -q)

from_pts :: ℝ2 -> ℝ2 -> Line
from_pts (x1, y1) (x2, y2) = Line slope intercept transpose
  where
    transpose = abs (y2 - y1) > abs (x2 - x1)
    slope
      | transpose = (x2 - x1) / (y2 - y1)
      | otherwise = (y2 - y1) / (x2 - x1)
    intercept
      | transpose = x1 - slope*y1
      | otherwise = y1 - slope*x1

parallel :: ℝ -> Line -> Line
parallel d (Line k q transpose) = Line k q' transpose
  where q' = d * sqrt (k^2 + 1) + q

implicit_fn :: Line -> ℝ2 -> ℝ
implicit_fn (Line k q False) = \(x, y) -> k*x + q - y
implicit_fn (Line k q True) = \(x, y) -> k*y + q - x

cut_below :: Line -> SymbolicObj2 -> SymbolicObj2
cut_below line = Yacad.ImplicitFn.intersect$ implicit_fn line

cut_above :: Line -> SymbolicObj2 -> SymbolicObj2
cut_above line = Yacad.ImplicitFn.intersect$ negate . implicit_fn line

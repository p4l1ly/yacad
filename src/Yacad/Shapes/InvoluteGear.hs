module Yacad.Shapes.InvoluteGear where

import Data.Fixed

import Graphics.Implicit
import Graphics.Implicit.Primitives
import Graphics.Implicit.Definitions

involute x y = - (x * cos angle + y * sin angle - 1)
  where angle = sqrt$ abs$ x^2 + y^2 - 1

circ x y = x^2 + y^2 - 1

invcirc :: SymbolicObj2
invcirc = flip implicit ((-5, -5), (5, 5)) $ \(x, y) -> maximum
  [ min (involute x y) (maximum [x - 1, y - 1.5, -x - 3])
  , -y
  , circ (x/4.9) (y/4.9)
  ]

involuteGear :: ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> SymbolicObj2
involuteGear baseScale pressureAngle modulus toothCount clearance =
  scale scale_ $ intersect
    [ circle (1 + outerRadiusCoef*2/toothCount)
    , union$ [circle (1 - baseRadiusCoef*2/toothCount)] ++
        [ rotate (i*toothAngle) $
            intersect [toothHalf, rotate toothAngle$ scale (1, -1) toothHalf]
        | i <- [0..toothCount - 1]
        ]
    ]

  where
    scale_ = double$ modulus * toothCount / 2

    (outerRadiusCoef, baseRadiusCoef) = (1, baseScale)

    toothHalf = rotate halfGapAngle$ scale (double involuteScale) invcirc
    involuteScale = cos pressureAngle

    toothAngle = 2 * pi / toothCount
    halfGapAngle = (toothAngle / 2 - 2 * (tan pressureAngle - pressureAngle)) / 2
      + clearance/toothCount

double :: a -> (a, a)
double x = (x, x)

baseRadius :: ℝ -> ℝ -> ℝ -> ℝ
baseRadius baseRatio toothCount modulus =
  modulus * toothCount / 2 * (1 - 2 * baseRatio / toothCount)

outerRadius :: ℝ -> ℝ -> ℝ
outerRadius toothCount modulus =
  modulus * toothCount / 2 * (1 + 2 / toothCount)

cone_fn :: ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> ℝ3 -> ℝ
cone_fn baseScale pressureAngle toothCount clearance k =
  \(xp, yp, zp) ->
    let
      ap = atan2 yp xp
      rp = sqrt$ xp^2 + yp^2
      re = (zp + 1/k*rp) / (k + 1/k)
      ze = k*re
      rp' = re + signum (ze - zp) * sqrt((rp - re)^2 + (zp - ze)^2)
      xp' = rp' * sin ap
      yp' = rp' * cos ap

      modulus
        | re < 0.1 = 2*0.1/toothCount
        | otherwise = 2*re/toothCount
      gear_fn = getImplicit$ involuteGear
        baseScale pressureAngle modulus toothCount clearance
    in
      gear_fn (xp', yp')

linearInvoluteGear :: ℝ -> ℝ -> ℝ -> ℝ -> ℝ2 -> ℝ
linearInvoluteGear baseScale pressureAngle modulus clearance = \(x, y) ->
  min (y + modulus*baseScale)$
  max (y - modulus)$
    let x' = mod' (x + period/2) period - period/2
    in max (y - k*x' - q) (y + k*x' - q)
  where
    k = 1 / tan pressureAngle
    q = k*modulus*(pi/2 - clearance)/2
    period = modulus*pi

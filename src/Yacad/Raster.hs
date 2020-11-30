{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Yacad.Raster where

import Data.List (intercalate)

import Control.Arrow
import qualified Data.Array as A
import Data.Array ((!))
import Data.Ix

import TH.Derive
import Data.Store

import Data.List.Ordered
import qualified Graphics.Implicit as Cad
import Graphics.Implicit.Definitions

data Raster2 = Raster2
  { resolution :: ℝ2
  , raster :: A.Array (Int, Int) Bool
  }
  deriving Show

$($(derive [d|instance Deriving (Store Raster2)|]))

box :: Raster2 -> (ℝ2, ℝ2)
box (Raster2 (xr, yr) (A.bounds -> ((x1, y1), (x2, y2)))) =
  ( (xr * fromIntegral x1, yr * fromIntegral y1)
  , (xr * fromIntegral x2, yr * fromIntegral y2)
  )

implicit_fn :: Raster2 -> ℝ2 -> ℝ
implicit_fn (Raster2 (xr, yr) raster) = \(x, y) ->
  let
    ix = (round$ x/xr, round$ y/yr)
  in
  if inRange bounds ix && raster!ix
    then -1
    else 1
  where bounds = A.bounds raster

implicit :: Raster2 -> SymbolicObj2
implicit raster = Cad.implicit (implicit_fn raster) (box raster)

blank :: ℝ2 -> (ℝ2, ℝ2) -> Raster2
blank res@(xr, yr) ((x1, y1), (x2, y2)) =
  Raster2{resolution = res, raster = A.listArray bnds$ repeat False}
  where bnds = ((floor$ x1/xr, floor$ y1/yr), (ceiling$ x2/xr, ceiling$ y2/yr))

full :: ℝ2 -> (ℝ2, ℝ2) -> Raster2
full res@(xr, yr) ((x1, y1), (x2, y2)) =
  Raster2{resolution = res, raster = A.listArray bnds$ repeat True}
  where bnds = ((floor$ x1/xr, floor$ y1/yr), (ceiling$ x2/xr, ceiling$ y2/yr))

(//) :: Raster2 -> [(ℝ2, Bool)] -> Raster2
(//) old@(Raster2 res raster) xs = old
  {raster = raster A.// filter_valid (map (first$ raster_ix res) xs)}
  where filter_valid = filter$ (inRange$ A.bounds raster) . fst

(//@) :: Raster2 -> [((Int, Int), Bool)] -> Raster2
(//@) old@(Raster2 _ raster) xs = old{raster = raster A.// filter_valid xs}
  where filter_valid = filter$ (inRange$ A.bounds raster) . fst

adjust :: ℝ2 -> ℝ2 -> ℝ2
adjust (xr, yr) = \(x, y) -> (x / xr, y / yr)

raster_ix :: ℝ2 -> ℝ2 -> (Int, Int)
raster_ix (xr, yr) = \(x, y) -> (round$ x / xr, round$ y / yr)

shell :: ℝ2 -> [ℝ2] -> (ℝ2 -> ℝ) -> [ℝ2]
shell res@(xr, yr) frontier0 fn =
  map (\(x, y) -> (xr * fromIntegral x, yr * fromIntegral y))
  $ concat$ takeWhile (not . null)$ map snd stages
  where
    frontier = sort$ map (raster_ix res) frontier0
    stages = flip iterate ([], frontier)$ \(old, current) ->
      let
        seen = merge old current
        new = nub$ sort$ concatMap surrounding current
        new' = filter isBorder$ minus new seen
      in
        (current, new')

    isBorder (x, y) = (fn first_corner > 0) `elem` map (\p -> fn p <= 0) other_corners
      where
        (x', y') = (xr * fromIntegral x, yr * fromIntegral y)
        first_corner = (x' - xrh, y' - yrh)
        other_corners =
          [ (x' - xrh, y' + yrh)
          , (x' + xrh, y' - yrh)
          , (x' + xrh, y' + yrh)
          ]

    xrh = xr/2
    yrh = yr/2

fill :: ℝ2 -> [ℝ2] -> (ℝ2 -> ℝ) -> [ℝ2]
fill res@(xr, yr) frontier0 fn =
  map (\(x, y) -> (xr * fromIntegral x, yr * fromIntegral y))
  $ concat$ takeWhile (not . null)$ map snd stages
  where
    frontier = sort$ map (raster_ix res) frontier0
    stages = flip iterate ([], frontier)$ \(old, current) ->
      let
        seen = merge old current
        new = nub$ sort$ concatMap surrounding current
        new' = filter isInside$ minus new seen
      in
        (current, new')

    isInside (x, y) = fn (xr * fromIntegral x, yr * fromIntegral y) <= 0

window :: (ℝ2 -> Bool -> [ℝ2]) -> Raster2 -> Raster2
window pixel_to_pixels (Raster2 res@(xr, yr) arr) = Raster2 res$
  A.listArray bnds (repeat False) A.// concatMap pos_to_pixels (range bnds)
  where
    bnds = A.bounds arr
    pos_to_pixels p@(x, y) = filter (inRange bnds . fst)
      $ map (\(x, y) -> ((round (x/xr), round (y/yr)), True))
      $ pixel_to_pixels p' (arr!p)
      where p' = (xr * fromIntegral x, yr * fromIntegral y)

window_int :: ((Int, Int) -> Bool -> [(Int, Int)]) -> Raster2 -> Raster2
window_int pixel_to_pixels rast@(Raster2 _ arr) = rast
  {raster = A.listArray bnds (repeat False) A.// concatMap pos_to_pixels (range bnds)}
  where
    bnds = A.bounds arr
    pos_to_pixels p = map (,True)$ filter (inRange bnds)$ pixel_to_pixels p (arr!p)

apply_mask :: [ℝ2] -> Raster2 -> Raster2
apply_mask mask rast@(Raster2 res@(xr, yr) _) = window_int pixel_to_pixels rast
  where
    mask_int = map (\(x, y) -> (round (x/xr), round (y/yr))) mask
    pixel_to_pixels _ False = []
    pixel_to_pixels (px, py) _ = map ((+px)***(+py)) mask_int

dilate :: ℝ -> Raster2 -> Raster2
dilate r rast@(Raster2 res _) = apply_mask mask rast
  where mask = fill res [(0, 0)] (\(x, y) -> x^2 + y^2 - r^2)

surrounding :: (Int, Int) -> [(Int, Int)]
surrounding (x, y) =
  [ (x + dx, y + dy)
  | (dx, dy) <-
      [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]
  ]

example_shell = blank (0.1, 0.1) ((-1.3, -1.3), (1.3, 1.3)) //
  map (, True) (shell (0.05, 0.05) [(1, 0)] (\(x, y) -> x^2 + y^2 - 1))

example_fill = blank (0.1, 0.1) ((-1.3, -1.3), (1.3, 1.3)) //
  map (, True) (fill (0.05, 0.05) [(0, 0)] (\(x, y) -> x^2 + y^2 - 1))

example_dilate = dilate 0.2 example_shell

showArr :: A.Array (Int, Int) Bool -> String
showArr arr = intercalate "\n" $
  flip map [y1..y2]$ \y ->
  flip map [x1..x2]$ \x -> if arr A.! (y, x) then '#' else ' '
  where ((y1, x1), (y2, x2)) = A.bounds arr

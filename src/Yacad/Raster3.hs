{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Yacad.Raster3 where

import Yacad.Raster.Expr

import Debug.Trace
import Data.List (intercalate)

import Control.Arrow
import qualified Data.Array as A
import Data.Array ((!))
import Data.Ix

import Data.List.Ordered
import qualified Graphics.Implicit as Cad
import Graphics.Implicit.Definitions
import Graphics.Implicit.ObjectUtil (getBox3, getImplicit3)

data Raster3 = Raster3
  { resolution :: ℝ3
  , raster :: A.Array (Int, Int, Int) Bool
  }
  deriving Show

box :: Raster3 -> (ℝ3, ℝ3)
box (Raster3 (xr, yr, zr) (A.bounds -> ((x1, y1, z1), (x2, y2, z2)))) =
  ( (xr * fromIntegral x1, yr * fromIntegral y1, zr * fromIntegral z1)
  , (xr * fromIntegral x2, yr * fromIntegral y2, zr * fromIntegral z2)
  )

implicit_fn :: Raster3 -> ℝ3 -> ℝ
implicit_fn (Raster3 (xr, yr, zr) raster) = \(x, y, z) ->
  let
    ix = (floor$ x/xr, floor$ y/yr, floor$ z/zr)
  in
  if inRange bounds ix && raster!ix
    then -1
    else 1
  where bounds = A.bounds raster

implicit :: Raster3 -> SymbolicObj3
implicit raster = Cad.implicit (implicit_fn raster) (box raster)

blank :: ℝ3 -> (ℝ3, ℝ3) -> Raster3

blank res box =
  Raster3{resolution = res, raster = A.listArray bnds$ repeat False}
  where bnds = bounds res box

full :: ℝ3 -> (ℝ3, ℝ3) -> Raster3
full res box =
  Raster3{resolution = res, raster = A.listArray bnds$ repeat True}
  where bnds = bounds res box

fromImplicit :: ℝ3 -> (ℝ3, ℝ3) -> Obj3 -> Raster3
fromImplicit res box obj = 
  Raster3{resolution = res, raster = A.listArray bnds$ map (\pos -> obj pos <= 0) $ boxPoints res bnds}
  where
    bnds = bounds res box
      
bounds :: ℝ3 -> (ℝ3, ℝ3) -> ((Int, Int, Int), (Int, Int, Int))
bounds res box = mapTuple (raster_ix res) box

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

boxPoints :: ℝ3 -> ((Int, Int, Int), (Int, Int, Int)) -> [ℝ3]
boxPoints (xr, yr, zr) ((xi1, yi1, zi1), (xi2, yi2, zi2)) =
  [ ((fromIntegral x+0.5)*xr, (fromIntegral y+0.5)*yr, (fromIntegral z+0.5)*zr) 
  | x <- [xi1..xi2], y <- [yi1..yi2], z <- [zi1..zi2]
  ]

rasterize :: ℝ3 -> SymbolicObj3 -> Raster3
rasterize res obj = fromImplicit res (getBox3 obj) (getImplicit3 obj)

(//) :: Raster3 -> [(ℝ3, Bool)] -> Raster3
(//) old@(Raster3 res raster) xs = old
  {raster = raster A.// filter_valid (map (first$ raster_ix res) xs)}
  where filter_valid = filter$ (inRange$ A.bounds raster) . fst

(//@) :: Raster3 -> [((Int, Int, Int), Bool)] -> Raster3
(//@) old@(Raster3 _ raster) xs = old{raster = raster A.// filter_valid xs}
  where filter_valid = filter$ (inRange$ A.bounds raster) . fst

adjust :: ℝ3 -> ℝ3 -> ℝ3
adjust (xr, yr, zr) = \(x, y, z) -> (x / xr, y / yr, z/zr)

raster_ix :: ℝ3 -> ℝ3 -> (Int, Int, Int)
raster_ix (xr, yr, zr) = \(x, y, z) -> (floor$ x / xr, floor$ y / yr, floor$ z / zr)

shell :: ℝ3 -> [ℝ3] -> (ℝ3 -> ℝ) -> [ℝ3]
shell res@(xr, yr, zr) frontier0 fn =
  map (\(x, y, z) -> (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z))
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

    isBorder (x, y, z) = (fn first_corner > 0) `elem` map (\p -> fn p <= 0) other_corners
      where
        (x', y', z') = (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z)
        first_corner = (x' - xrh, y' - yrh, z' - zrh)
        other_corners = drop 1 [(sx x' xrh, sy y' yrh, sz z' zrh) | sx <- signs, sy <- signs, sz <- signs]
          where
            signs = [(-), (+)]

    xrh = xr/2
    yrh = yr/2
    zrh = zr/2

fill :: ℝ3 -> [ℝ3] -> (ℝ3 -> ℝ) -> [ℝ3]
fill res@(xr, yr, zr) frontier0 fn =
  map (\(x, y, z) -> (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z))
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

    isInside (x, y, z) = fn (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z) <= 0

fillBox :: ℝ3 -> Box3 -> Obj3 -> [ℝ3]
fillBox res box obj = filter (\pos -> obj pos <= 0)$ boxPoints res$ bounds res box

fillObj :: ℝ3 -> SymbolicObj3 -> [ℝ3]
fillObj res obj = fillBox res (getBox3 obj) (getImplicit3 obj)

result :: (b -> b') -> ((a -> b) -> (a -> b'))
result =  (.)
swap_1_2 = flip    
swap_2_3 = result flip
shl_3 = swap_2_3.swap_1_2

fillObjE :: SymbolicObj3 -> Expr (ℝ3 -> [ℝ3])
fillObjE obj = Obj [flip fillObj obj]

fillBoxE :: Box3 -> Obj3 -> Expr (ℝ3 -> [ℝ3])
fillBoxE box obj = Obj [shl_3 fillBox box obj]

fillE :: [ℝ3] -> (ℝ3 -> ℝ) -> Expr (ℝ3 -> [ℝ3])
fillE frontier0 fn = Obj [shl_3 fill frontier0 fn]

translateE :: ℝ3 -> (ℝ3 -> ℝ3)
translateE v = (+v)

rotateE :: ℝ3 -> (ℝ3 -> ℝ3)
rotateE (yz, zx, xy) =
  let
        rotateYZ :: ℝ -> ℝ3 -> ℝ3
        rotateYZ θ (x,y,z) = ( x, y*cos θ - z*sin θ, z*cos θ + y*sin θ)
        rotateZX :: ℝ -> ℝ3 -> ℝ3
        rotateZX θ (x,y,z) = ( x*cos θ + z*sin θ, y, z*cos θ - x*sin θ)
        rotateXY :: ℝ -> ℝ3 -> ℝ3
        rotateXY θ (x,y,z) = ( x*cos θ - y*sin θ, y*cos θ + x*sin θ, z)
    in
        rotateXY xy . rotateZX zx . rotateYZ yz

scaleE :: ℝ3 -> (ℝ3 -> ℝ3)
scaleE v = (*v)

infixr 1 <~
(<~) :: (ℝ3 -> ℝ3) -> Expr (ℝ3 -> [ℝ3]) -> Expr (ℝ3 -> [ℝ3])
(<~) f (Obj fns) = Obj$ map (\fn -> (\res -> map f $ fn res)) fns
(<~) f (Union exprs) = Union$ map (f<~) exprs
(<~) f (Diff exprs) = Diff$ map (f<~) exprs

modify :: Raster3 -> Expr (ℝ3 -> [ℝ3]) -> Raster3
modify old@(Raster3 res _) expr = old // do
  ((f, add), i) <- zip (run expr) [1..]
  p <- trace (show i)$ f res
  return (p, add)

window :: (ℝ3 -> Bool -> [ℝ3]) -> Raster3 -> Raster3
window pixel_to_pixels (Raster3 res@(xr, yr, zr) arr) = Raster3 res$
  A.listArray bnds (repeat False) A.// concatMap pos_to_pixels (range bnds)
  where
    bnds = A.bounds arr
    pos_to_pixels p@(x, y, z) = filter (inRange bnds . fst)
      $ map (\(x, y, z) -> ((floor (x/xr), floor (y/yr), floor (z/zr)), True))
      $ pixel_to_pixels p' (arr!p)
      where p' = (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z)

window_int :: ((Int, Int, Int) -> Bool -> [(Int, Int, Int)]) -> Raster3 -> Raster3
window_int pixel_to_pixels rast@(Raster3 _ arr) = rast
  {raster = A.listArray bnds (repeat False) A.// concatMap pos_to_pixels (range bnds)}
  where
    bnds = A.bounds arr
    pos_to_pixels p = map (,True)$ filter (inRange bnds)$ pixel_to_pixels p (arr!p)

apply_mask :: [ℝ3] -> Raster3 -> Raster3
apply_mask mask rast@(Raster3 res@(xr, yr, zr) _) = window_int pixel_to_pixels rast
  where
    mask_int = map (\(x, y, z) -> (floor (x/xr), floor (y/yr), floor (z/zr))) mask
    pixel_to_pixels _ False = []
    pixel_to_pixels (px, py, pz) _ = map (\(x, y, z) -> (x + px, y + py, z + pz)) mask_int

dilate :: ℝ -> Raster3 -> Raster3
dilate r rast@(Raster3 res _) = apply_mask mask rast
  where mask = fill res [(0, 0, 0)] (\(x, y, z) -> x^2 + y^2 +z^2 - r^2)

surrounding :: (Int, Int, Int) -> [(Int, Int, Int)]
surrounding (x, y, z) =
  [ (x + dx, y + dy, z + dz)
  | dx <- ds, dy <- ds, dz <- ds, dx /= 0 || dy /= 0 || dz /= 0
  ]
  where
    ds = [-1, 0, 1]

example_shell = blank (0.1, 0.1, 0.1) ((-1.3, -1.3, -1.3), (1.3, 1.3, 1.3)) //
  map (, True) (shell (0.05, 0.05, 0.05) [(1, 0, 0)] (\(x, y, z) -> x^2 + y^2 + z^2 - 1))

example_fill = blank (0.1, 0.1, 0.1) ((-1.3, -1.3, -1.3), (1.3, 1.3, 1.3)) //
  map (, True) (fill (0.05, 0.05, 0.05) [(0, 0, 0)] (\(x, y, z) -> x^2 + y^2 + z^2 - 1))

example_dilate = dilate 0.2 example_shell

showArr :: A.Array (Int, Int) Bool -> String
showArr arr = intercalate "\n" $
  flip map [y1..y2]$ \y ->
  flip map [x1..x2]$ \x -> if arr A.! (y, x) then '#' else ' '
  where ((y1, x1), (y2, x2)) = A.bounds arr

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}

module Yacad.Raster.Expr where

import Data.Functor.Foldable

-- Boilerplate begins here

data ExprF point rec
  = UnionF [rec]
  | DiffF [rec]
  | ObjF [point]
  deriving (Functor, Show)

type Expr point = Fix (ExprF point)

pattern Union xs = Fix (UnionF xs)
pattern Diff xs = Fix (DiffF xs)
pattern Obj pts = Fix (ObjF pts)

-- Boilerplate end
-------------------------------------------------------------------
-- Real logic begins here

run :: Expr point -> [(point, Bool)]
run = elgot concat coalg . (True,)

coalg :: (Bool, Expr point) -> Either [(point, Bool)] [(Bool, Expr point)]
coalg (add, Union xs)    = Right$ map (add,) xs
coalg (add, Diff (x:xs)) = Right$ (add, x) : map (not add,) xs
coalg (add, Obj pts)     = Left$ map (, add) pts

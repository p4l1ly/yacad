{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Yacad.Export.Export where

import Yacad.Raster3 as Ra3

import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.ByteString.Lazy as BL
import Data.Store ( encode )
import qualified Data.ByteString.Builder as BB
import qualified Codec.Picture as Pic
import qualified Text.Printf as PF
import Text.XML.HXT.Core
import Text.Scanf
import System.Directory
import Control.Monad
import Debug.Trace as Debug

writeRa3 :: String -> Raster3 -> IO ()
writeRa3 name (Raster3 _ ra) = 
  let
    ((x1, y1, z1), (x2, y2, z2)) = A.bounds ra
    (sx, sy, sz) = (x2-x1, y2-y1, z2-z1)
  in
  BL.writeFile name $ BB.toLazyByteString
      $ BB.int32BE (fromIntegral$ sx) <> BB.int32BE (fromIntegral$ sy) <> BB.int32BE (fromIntegral$ sz)
      <> (BB.byteString$ encode ra)

writeSVX :: Bool -> String -> Raster3 -> IO ()
writeSVX brokenSlicesOr name (Raster3 (rx, _, _) ra) =
  let
    box@((x1, y1, z1), (x2, y2, z2)) = A.bounds ra
    size@(sx, sy, sz) = (x2-x1, y2-y1, z2-z1)
    boxf@((_, y1f, z1f), (_, y2f, z2f)) = mapTuple (fixSlicesOr brokenSlicesOr)$ box
    sizef@(sxf, syf, szf) = (fixSlicesOr brokenSlicesOr) size
  in
    do
      exists <- doesDirectoryExist name
      if exists then removeDirectoryRecursive name else return ()
      createDirectory name
      createDirectory$ name++"/density"
      sequence_ (map (\z -> Pic.savePngImage (PF.printf "%s/density/slice%04d.png" name z)
                  $ Pic.ImageY8$ Pic.generateImage (\x y -> if ra!(x1+x, y1+y, z1+z) then 255 else 0) sx sy) [0..sz])
      BL.writeFile (name++"/manifest.xml")$ BB.toLazyByteString$ BB.stringUtf8
        $ PF.printf
            "<?xml version=\"1.0\"?>\n\
            \<grid version=\"1.0\" gridSizeX=\"%d\" gridSizeY=\"%d\" gridSizeZ=\"%d\" \n\
            \ originX=\"%f\" originY=\"%f\" originZ=\"%f\"\n\
            \ voxelSize=\"%f\" subvoxelBits=\"8\" slicesOrientation=\"Z\" >\n\
            \    <channels>\n\
            \        <channel type=\"DENSITY\" bits=\"8\" slices=\"density/slice%s.png\" />\n\
            \    </channels>\n\n\
            \    <materials>\n\
            \        <material id=\"1\" urn=\"urn:shapeways:materials/1\" />\n\
            \    </materials>\n\n\
            \    <metadata>\n\
            \        <entry key=\"author\" value=\"Yacad\" />\n\
            \        <entry key=\"creationDate\" value=\"2020/11/29\" />\n\
            \    </metadata>\n\
            \</grid>"
            (sx+1) (syf+1) (szf+1)
            (fromIntegral x1*rx*0.001) (fromIntegral y1f*rx*0.001) (fromIntegral z1f*rx*0.001)
            (rx/1000) "%04d"

fixSlicesOr :: Bool -> (a, a, a) -> (a, a, a)
fixSlicesOr False = id
fixSlicesOr True = \(x, y, z) -> (x, z, y)

--TODO: use for export as well
--TODO: attrs from list (foldl)
-- manifestAttrs :: [String]
-- manifestAttrs = ["gridSizeX", "gridSizeY", "gridSizeZ", "originX", "originY", "originZ", "voxelSize"]
xpRaster :: Bool -> PU Raster3
xpRaster brokenSlicesOr
    = xpElem "grid"$ 
      xpFilterAttr (
        hasName "gridSizeX" 
                <+> hasName "gridSizeY"
                <+> hasName "gridSizeZ"
                <+> hasName "originX"
                <+> hasName "originY"
                <+> hasName "originZ"
                <+> hasName "voxelSize"
                )$
      xpFilterCont (none)$
      xpWrap 
        ( \(sx, sy, sz, x, y, z, r) -> 
              let
                (xx, yy, zz, rr) = (x, y, z, r)*1000
              in
              blank (rr, rr, rr)$ mapTuple (fixSlicesOr brokenSlicesOr) ((xx+rr*0.5, yy+rr*0.5, zz+rr*0.5), (xx+(fromIntegral sx-0.5)*rr, yy+(fromIntegral sy-0.5)*rr, zz+(fromIntegral sz-0.5)*rr))
        , \(Raster3 ((fixSlicesOr brokenSlicesOr) -> (rx, ry, rz)) (A.bounds -> ((x1, y1, z1), (x2, y2, z2)))) -> 
              (x2-x1, y2-y1, z2-z1, fromIntegral x1*rx, fromIntegral y1*ry, fromIntegral z1*rz, rx*0.001)
        )$
      xp7Tuple (xpAttr "gridSizeX" xpPrim)
               (xpAttr "gridSizeY" xpPrim)
               (xpAttr "gridSizeZ" xpPrim)
               (xpAttr "originX"   xpPrim)
               (xpAttr "originY"   xpPrim)
               (xpAttr "originZ"   xpPrim)
               (xpAttr "voxelSize" xpPrim)

readSVX :: Bool -> String -> IO Raster3
readSVX brokenSlicesOr name = 
  do
    [ra] <- runX 
                  ( xunpickleDocument (xpRaster brokenSlicesOr)
                                [ withValidate no
                                , withTrace 1
                                , withRemoveWS yes
                                , withPreserveComment no
                                ]$ name ++ "/manifest.xml"
                  )
    files <- getDirectoryContents$ dirName
    slices <- Debug.trace (show$ A.bounds$ raster ra)$ fillSlices (A.bounds$ raster ra) files
    return$ ra {raster = (raster ra) A.// (map (, True)$ slices)}
  where
    dirName :: String
    dirName = name ++ "/density/"

    fillSlices :: ((Int, Int, Int), (Int, Int, Int)) -> [FilePath] -> IO [(Int, Int, Int)]
    fillSlices ((x1, y1, z1), (x2, y2, _)) files = do
        slices <- sequence$ map fillSlice files
        return$ concat slices
      where
        fillSlice :: FilePath -> IO [(Int, Int, Int)]
        fillSlice file@(scanf [fmt|slice%d.png|] -> parsed) = 
          case parsed of
            Nothing -> return []
            (Just (z :+ ())) -> do
              imgLoading <- Pic.readPng$ dirName ++ file
              case imgLoading of
                Left err -> return$ Debug.trace ("error loading " ++ file ++ ": " ++ err)$ []
                Right (Pic.ImageY8 img) ->
                  return$ map (\(x, y) -> (x1 + x, y1 + y, z1 + z))$
                          filter (\(x, y) -> Pic.pixelAt img x y /= 0)
                          [(x, y) | x <- [0..x2-x1-1], y <- [0..y2-y1-1]]
                Right _ -> return$ Debug.trace ("Unsupported image format " ++ file)$ []




--TODO
-- https://getqubicle.com/qubicle/documentation/docs/file/qb/
-- B.writeFile "test.qb"
--   $ BB.toLazyByteString
--      $ BB.word8 1 <> BB.word8 1 <> BB.word8 0 <> BB.word8 0
--     <> BB.int32BE 0 <> BB.int32BE 1 <> BB.int32BE 0 <> BB.int32BE 0 <> BB.int32BE 1
--     <> BB.word8 3 <> BB.stringUtf8 "obj"
--     <> BB.int32BE (fromIntegral$ x2-x1) <> BB.int32BE (fromIntegral$ y2-y1) <> BB.int32BE (fromIntegral$ z2-z1)
--     <> BB.int32BE (fromIntegral$ x1) <> BB.int32BE (fromIntegral$ y1) <> BB.int32BE (fromIntegral$ z1)
--     <> --(BB.byteString$ runPut . mapM_ (\p -> putWord8$ if sm p <= 0 then 1 else 255)$ boxPoints res box)

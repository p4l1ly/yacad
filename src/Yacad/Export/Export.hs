module Yacad.Export.Export where

import Yacad.Raster3 as Ra3

import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.ByteString.Lazy as BL
import Data.Store ( encode )
import qualified Data.ByteString.Builder as BB
import qualified Codec.Picture as Pic
import Text.Printf
import System.Directory

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
    ((x1, y1, z1), (x2, y2, z2)) = A.bounds ra
    (sx, sy, sz) = (x2-x1, y2-y1, z2-z1)
  in
    do
      exists <- doesDirectoryExist name
      if exists then removeDirectoryRecursive name else return ()
      createDirectory name
      createDirectory$ name++"/density"
      sequence_ (map (\z -> Pic.savePngImage (printf "%s/density/slice%04d.png" name z)
                  $ Pic.ImageY8$ Pic.generateImage (\x y -> if ra!(x1+x, y1+y, z1+z) then 255 else 0) sx sy) [0..sz])
      BL.writeFile (name++"/manifest.xml")$ BB.toLazyByteString$ BB.stringUtf8
        $ printf
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
            \        </metadata>\n\
            \</grid>"
            (sx+1) ((if brokenSlicesOr then sz else sy)+1) ((if brokenSlicesOr then sy else sz)+1)
            (fromIntegral x1*rx/1000)
            (fromIntegral (if brokenSlicesOr then z1 else y1)*rx/1000) (fromIntegral (if brokenSlicesOr then y1 else z1)*rx/1000)
            (rx/1000) "%04d"

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

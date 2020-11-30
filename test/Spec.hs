-- import Yacad.Raster as Ra2
import Yacad.Raster3 as Ra3
import Yacad.Raster.Expr
import Yacad.Export.Export
import Graphics.Implicit
import Debug.Trace
import Control.DeepSeq
import System.CPUTime
import Data.Time
import Text.Printf

instance NFData Raster3 where
  rnf r = rnf (Ra3.resolution r) `seq` rnf (Ra3.raster r)

main :: IO ()
main = 
  let
  --   chunkCnt = (sx`div`255)*(sy`div`255)*(sz`div`255)
  in
  do
  -- writeSVG 0.1 "test.svg"$ Ra2.implicit Ra2.example_shell
  -- writeSTL 0.1 "test-rasterize.stl"$ Ra3.implicit$ Ra3.rasterize 0.1
  --   $ union
  --     [ sphere 1.2
  --     , translate (0, 0, 2.1)$ sphere 1
  --     , translate (0, 0, 2.1)
  --         $ rotate3 (0, pi/2, 0) 
  --             $ extrudeRM 0 (Left 0) (Right (\z -> 1-z*0.5)) (Left (0, 0)) (circle 0.3) (Left 2)
  --     , translate (0, 0, 3.1)
  --         $ extrudeOnEdgeOf 
  --               (polygonR 0 [(0.3, 0), (0.3, 0.2), (0, 0.2), (0, 1), (-0.5, 1), (-0.5, 0)])
  --             $ circle(0.5)
  --     ]
    start <- trace "\n"$ getCPUTime
    end <- snowman `deepseq` getCPUTime
    trace (printf "\nraster filling: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- getCPUTime
    trace "\nra3"$ writeRa3 "test.ra3" snowman
    end <- getCPUTime
    trace (printf "\nra3 export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- getCPUTime
    trace "svx"$ writeSVX True "testModel" snowman
    end <- getCPUTime
    trace (printf "\nsvx export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- getCPUTime
    trace "stl"$ writeSTL 0.1 "test-drawing.stl"$ Ra3.implicit$ snowman
    end <- getCPUTime
    trace (printf "\nstl export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()
    

    

snowman = modify (Ra3.blank 0.02 ((-1.5, -1.2, -1.35), (2.0, 1.2, 4.2))) (-0.0001)$ Union
        [ Diff
            [ Ra3.fillObjE$ sphere 1.2
            , Ra3.fillObjE$ rect3R 0 (0, 0, 0) (2, 2, 2)
            ]
        -- , translateE (0, 0.5, 2.1) <~ rotateE (0, -pi/2, pi/2) <~ Union 
        , translateE (0, 0, 2.1) <~ Union 
            [ Diff
                [ Ra3.fillObjE$ sphere 1
                , Ra3.fillObjE$ rect3R 0 (-2.5) 0 -- FIXME: the cube is deleted from lower sphere as well...
                ]
            , Ra3.fillObjE
                $ rotate3 (0, pi/2, 0) 
                    $ extrudeRM 0 (Left 0) (Right (\z -> 1-z*0.5)) (Left (0, 0)) (circle 0.3) (Left 2)
            , Ra3.fillObjE$ translate (0, 0, 1)
                $ extrudeOnEdgeOf 
                      (polygonR 0 [(0.3, 0), (0.3, 0.2), (0, 0.2), (0, 1), (-0.5, 1), (-0.5, 0)])
                    $ circle(0.5)
            ]
        ]
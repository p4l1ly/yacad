import Yacad.Raster as Ra2
import Yacad.Raster3 as Ra3
import Graphics.Implicit

main :: IO ()
main = do
    writeSVG 0.1 "test.svg"$ Ra2.implicit Ra2.example_shell
    writeSTL 0.1 "test.stl"$ Ra3.implicit$ Ra3.rasterize 0.1
      $ union
        [ Ra3.implicit Ra3.example_dilate
        , translate (0, 0, 2.1) $ Ra3.implicit Ra3.example_fill
        , translate (0, 0, 2.1)
            $ rotate3 (0, pi/2, 0) 
                $ extrudeRM 0 (Left 0) (Right (\z -> 1-z*0.5)) (Left (0, 0)) (circle 0.3) (Left 2)
        , translate (0, 0, 3.1)
            $ extrudeOnEdgeOf 
                  (polygonR 0 [(0.3, 0), (0.3, 0.2), (0, 0.2), (0, 1), (-0.5, 1), (-0.5, 0)])
                $ circle(0.5)
        ]

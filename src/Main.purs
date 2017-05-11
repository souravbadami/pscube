module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array (fromFoldable, reverse)
import Data.Foldable (fold)
import Data.Maybe (fromJust)
import Data.Pair (Pair(..))
import Data.Traversable (sequence)
import Graphics.Canvas (CANVAS, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D)
import Graphics.Drawing
import Partial.Unsafe (unsafePartial)

main :: Eff (canvas :: CANVAS) Unit
main = do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    width <- getCanvasWidth canvas
    height <- getCanvasHeight canvas
    ctx <- getContext2D canvas

    let style = outlineColor black <> lineWidth 2.0

    render ctx $
        translate 200.0 200.0 $
                drawCube3D (cube3D 50.0) style \{ x, y, z } -> { x: x + 0.5*z, y: y + 0.5*z }

cube3D :: Number -> Array (Pair { x :: Number, y :: Number, z :: Number })
cube3D max = do
    let min = -max
    let both = [min, max]
    x <- both
    y <- if x == min then reverse both else both
    pure $ Pair { x, y, z: min } { x, y, z: max }

drawCube3D ::
    Array (Pair { x :: Number, y :: Number, z :: Number }) ->
    OutlineStyle ->
    ({ x :: Number, y :: Number, z :: Number } -> Point) ->
    Drawing
drawCube3D cube3D style proj =
    fold $
        map (outlined style) $
            drawnFaces <> drawnSides
    where
        -- there are two layers, Array and Pair, to map through
        cube = map (map proj) cube3D
        -- flip these layers: Pair (Array C.Point)
        faces = sequence cube
        -- draw each face as a closed quadrilateral
        drawnFaces = fromFoldable $ map closed faces
        -- draw a line from a Pair
        line pair = path (fromFoldable pair)
        -- draw remaining sides, which span each pair in the array
        drawnSides = map line cube
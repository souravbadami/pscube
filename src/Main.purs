module Main where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Array ((..))
import Data.Monoid (mempty)
import Data.Foldable (fold)

import Control.Monad.Eff (Eff)

import Graphics.Drawing (scale, translate, shadowBlur, black, shadowColor,
                         shadow, render, rotate, closed, outlineColor, outlined, path)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)

import Math (sin, cos, pi)

import Color.Scale (sample)
import Color.Scale.Perceptual (magma)

main :: Eff (canvas :: CANVAS) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas

  render ctx $
      translate 400.0 200.0 $
          go 5

  where
  s = 0.375

  go 0 = mempty
  go n =
    let dr = scale s s (go (n - 1))
    in outlined (outlineColor (sample magma (1.0 - toNumber (n - 1) / 5.0))) cube

  cube = path [{ x: 0.0 * 100.0, y: 0.0 * 100.0  }, 
               { x: 1.0 * 100.0, y: 0.0 * 100.0  },
               { x: 1.0 * 100.0, y: 1.0 * 100.0  },
               { x: 0.0 * 100.0, y: 1.0 * 100.0  },
               { x: 0.0 * 100.0, y: 0.0 * 100.0  },
               { x: 0.5 * 100.0, y: -0.5 * 100.0 },
               { x: 1.5 * 100.0, y: -0.5 * 100.0 },
               { x: 1.0 * 100.0, y: 0.0 * 100.0  },
               { x: 1.5 * 100.0, y: -0.5 * 100.0 },
               { x: 1.5 * 100.0, y: 0.5 * 100.0 },
               { x: 1.0 * 100.0, y: 1.0 * 100.0  },
               { x: 0.0 * 100.0, y: 1.0 * 100.0  },
               { x: 0.5 * 100.0, y: 0.5 * 100.0  },
               { x: 0.5 * 100.0, y: -0.5 * 100.0  },
               { x: 0.5 * 100.0, y: 0.5 * 100.0  },
               { x: 1.5 * 100.0, y: 0.5 * 100.0  }]

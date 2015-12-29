{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TypeFamilies, RankNTypes, ParallelListComp #-}
module Main where

import Control.Lens (set)
import Control.Monad.Trans (liftIO)
import Data.Text (Text, pack, unpack)
import qualified Data.JSString as JSString
import GHCJS.Types (JSRef(..), JSString(..))
import Language.Haskell.HSX.QQ (hsx)
import qualified Data.Set as Set
import Data.Time.Calendar (Day, toGregorian, fromGregorian, diffDays, showGregorian)
import System.Random (randoms, mkStdGen)
import Web.ISO.HSX
import Web.ISO.Murv
import Web.ISO.Types

{- Model -}
data Model = Model
    { count :: Int
    , msg   :: Text
    }

{- Action -}
data Action
    = Increment
    | Decrement
    | Msg Text
    | Set (Maybe JSString)

instance Show Action where
    show Increment = "Increment"
    show Decrement = "Decrement"
    show (Msg txt) = "Msg " ++ unpack txt
    show (Set _)   = "Set _"

{- Update -}
update' :: Action -> Model -> (Model, Maybe Text)
update' action model =
    case action of
      Increment -> (model { count = (count model) + 1 }, Just "inc")
      Decrement -> (model { count = (count model) - 1 }, Just "dec")
      Msg txt   -> (model { msg = txt }, Nothing)
      Set (Just jstr)  ->
          case reads (JSString.unpack' jstr) :: [(Int, String)] of
            [(n, _)] -> (model { count = n }, Just "set")
            _ -> (model, Nothing)
      Set _ -> (model, Nothing)

data Scale
  = Linear
  | Log
    deriving (Eq, Show, Read)

scatterPlotDay :: Double -- ^ width in pixels
               -> Double -- ^ height in pixels
               -> Scale  -- ^ x-scale
               -> Scale  -- ^ y-scale
               -> [(Double, Canvas2D)] -- ^ y-axis labels, ascending order
               -> [(Day, Double)] -- ^ points
               -> Canvas2D
scatterPlotDay width height xScale yScale yLabels points =
  let days         = map fst points
      minDay       = minimum days -- FIXME: fails if list is empty
      numDays      = diffDays (maximum days) minDay -- FIXME: fails if list is empty
      dayToFloat d = fromIntegral (diffDays d minDay)
      dayLabel d   = WithContext2D [Font "18px Times", TextAlign AlignStart, Translate 0 20, Rotate (pi/2)] [ Draw (FillText (JSString.pack (showGregorian d)) 0 0 Nothing)]
      xLabels      = [ (dayToFloat d, dayLabel d) | (d ,_) <- points ]
      points'      = [ (dayToFloat d, v) | (d ,v) <- points ]
  in
   scatterPlot width height xScale xLabels yScale yLabels points'

scatterPlot :: Double -- ^ width in pixels
            -> Double -- ^ height in pixels
            -> Scale -- ^ x-scale
            -> [(Double, Canvas2D)] -- ^ x-axis labels, ascending order
            -> Scale -- ^ y-scale
            -> [(Double, Canvas2D)] -- ^ y-axis labels, ascending order
            -> [(Double, Double)] -- ^ points
            -> Canvas2D
scatterPlot width' height' xScale xLabels yScale yLabels points =
  let paddingTop = 20
      paddingBottom = 100
      paddingLeft = 110
      paddingRight = 20
      height = height' - (paddingTop + paddingBottom)
      width  = width'  - (paddingLeft + paddingRight)
      xMin = minimum (map fst xLabels)
      xMax = maximum (map fst xLabels)
      yMin = minimum (map fst yLabels)
      yMax = maximum (map fst yLabels)
      xDelta = xMax - xMin
      yDelta = yMax - yMin
      yOffset = height - yMin
      toYPos yVal = (height + paddingTop) - ((yVal - yMin) * (height / yDelta))
      toXPos xVal = paddingLeft + (xVal * (width / xDelta))
      drawXAxis height toXPos (x, label) =
        [ WithContext2D [Translate (toXPos x) height] [ label ]
        , Draw (Stroke [ MoveTo (toXPos x) (height+paddingTop)
                       , LineTo (toXPos x) paddingTop
                       ])
        ]
      drawYAxis width toYPos (y, label) =
        [ WithContext2D [Translate paddingLeft (toYPos y)] [ label ]
        , Draw (Stroke [ MoveTo (paddingLeft) (toYPos y)
                       , LineTo (paddingLeft + width) (toYPos y)
                       ])
        ]

  in WithContext2D []
       [ -- Draw (FillText "A Scatter Plot" 0.5 20.5 Nothing)
--       , Draw (FillText "A Scatter Plot" 1 40 Nothing)
        WithContext2D []
          (concat $ concat [ map (drawYAxis width toYPos) yLabels
                           , map (drawXAxis height toXPos) xLabels
                           ])
       , WithContext2D [FillStyle (StyleColor (ColorName "green"))]
         (concatMap (drawPoint toXPos toYPos) points)
       ]

  where
    drawPoint toXPos toYPos (x, y) =
      let arc = Arc (toXPos x) (toYPos y) 3.0 0 (2*pi) False
      in
      [ Draw (Stroke [ arc ])
      , Draw (Fill [ arc ])
      ]

{- View -}
view' :: Model -> (HTML Action, [Canvas])
view' (Model c txt) =
  let plus x y = Draw (Stroke [ MoveTo   (x - 10) y
                              , LineTo   (x + 10) y
                              , MoveTo x (y - 10)
                              , LineTo x (y + 10)
                              ])
      december = [ fromGregorian 2015 12 d | d <- [1..31]  ]
      points = [ (day, y*100) | day <- december | y <- take 31 (randoms (mkStdGen (c + 1)))]
--      canvas w h = scatterPlot w h Linear [ (x, WithContext2D [Font "18px Times", TextAlign AlignStart, Translate 0 20, Rotate (pi/2)] [ Draw (FillText (JSString.pack (show x)) 0 0 Nothing)]) | x <- [0, 20, 40, 60, 70, 80, 100]]
--                                   Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [0, 20, 40, 60, 70, 80, 100]] points
      canvas w h = scatterPlotDay w h
                     Linear
                     Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [0, 20, 40, 60, 70, 80, 100]] points

  in
   ([hsx| <div>
           <h1>Scatter Plot</h1>
           <canvas id="canvas" width="960" height="480"></canvas>

           <div>
            <p>The count is <% show c %></p>
            <button onclick=Decrement>-</button>
            <button onclick=Increment>+</button>
            <p><input type="text" oninput=Set value=(pack $ show c) /><% txt %></p>
           </div>

           <canvas id="canvas2" width="480" height="240"></canvas>

         </div>
        |], [Canvas "canvas" (canvas 960 480), Canvas "canvas2" (canvas 480 240)])


counter :: MURV Model Action Text
counter = MURV
  { model  = Model { count = 0
                   , msg = "Nothing to Say."
                   }
  , update = update'
  , view   = view'
  }

main :: IO ()
main = murv "http://localhost:8000/api" Msg counter Nothing

      
{-
v        (WithContext2D (set fillStyle (StyleColor (ColorName "green")) context2D)
           [ Draw (FillRect (Rect 0 0 10 10))
           , WithContext2D (set strokeStyle (StyleColor (ColorName "blue")) context2D)
              [ plus 10 10
              , plus 30 30
              , plus 50 50
              ]
          , Draw (FillRect (Rect 10 10 5 10))
          ])
-}

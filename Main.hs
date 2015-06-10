{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TypeFamilies, RankNTypes #-}
module Main where

import Control.Monad.Trans (liftIO)
import GHCJS.Types (JSRef(..))
import ISOHSX
import Language.Haskell.HSX.QQ (hsx)
import Types

{- Model -}
data Model = Model { count :: Int }

{- Update -}
data Action
    = Increment
    | Decrement

update' :: Action -> Model -> Model
update' action model =
    case action of
      Increment -> model { count = (count model) + 1 }
      Decrement -> model { count = (count model) - 1 }

{- View -}
view' :: Model -> HTML Action
view' (Model c) =
    [hsx| <div>
            <p>The Count is <% show c %></p>
            <button onclick=Decrement>-</button>
            <button onclick=Increment>+</button>
          </div>
        |]

counter :: MUV Model Action
counter = MUV
  { muvModel  = Model 0
  , muvUpdate = update'
  , muvView   = view'
  }

main :: IO ()
main = muv counter



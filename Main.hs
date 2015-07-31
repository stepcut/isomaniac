{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TypeFamilies, RankNTypes #-}
module Main where

import Control.Monad.Trans (liftIO)
import Data.Text (Text, pack, unpack)
import GHCJS.Foreign(fromJSString)
import GHCJS.Types (JSRef(..), JSString(..))
import Language.Haskell.HSX.QQ (hsx)
import Web.ISO.HSX
import Web.ISO.Murv
import Web.ISO.Types

{- Model -}
data Model = Model
    { count :: Int
    , msg   :: Text
    }

{- Update -}
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

update' :: Action -> Model -> (Model, Maybe Text)
update' action model =
    case action of
      Increment -> (model { count = (count model) + 1 }, Just "inc")
      Decrement -> (model { count = (count model) - 1 }, Just "dec")
      Msg txt   -> (model { msg = txt }, Nothing)
      Set (Just jstr)  ->
          case reads (fromJSString jstr) :: [(Int, String)] of
            [(n, _)] -> (model { count = n }, Just "set")
            _ -> (model, Nothing)
      Set _ -> (model, Nothing)

{- View -}
view' :: Model -> HTML Action
view' (Model c txt) =
    [hsx| <div>
            <p>The Count is <% show c %></p>
            <button onclick=Decrement>-</button>
            <button onclick=Increment>+</button>
            <input type="text" oninput=Set value=(pack $ show c) />
            <p><% txt %></p>
          </div>
        |]

counter :: MURV Model Action Text
counter = MURV
  { model  = Model 0 "Nothing to Say."
  , update = update'
  , view   = view'
  }

main :: IO ()
main = murv "http://localhost:8000/api" Msg counter

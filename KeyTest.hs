{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TypeFamilies, RankNTypes #-}
module Main where

import Control.Monad.Trans (liftIO)
import Data.Char (chr)
import Data.Text (Text, pack, unpack)
import qualified Data.JSString as JS
import GHCJS.Types (JSRef(..), JSString(..))
import Language.Haskell.HSX.QQ (hsx)
import Web.ISO.HSX
import Web.ISO.Murv
import Web.ISO.Types

{- Model -}
data Model = Model
    { char   :: Char
    }

{- Action -}
data Action
    = KeyPressed Char

instance Show Action where
  show (KeyPressed c) = "Keypressed " ++ show c

{- Update -}
update' :: Action -> Model -> (Model, Maybe Text)
update' action model =
    case action of
      KeyPressed c -> (model { char = c }, Nothing)

{- View -}
view' :: Model -> (HTML Action, [Canvas])
view' (Model c) =
    ([hsx| <div tabindex="1" [Event KeyPress (\e -> pure (KeyPressed (chr (charCode e))))]>
             <p>The last key that was pressed was: <% show c %></p>
           </div>
         |], [])

counter :: MURV Model Action Text
counter = MURV
  { model  = Model ' '
  , update = update'
  , view   = view'
  }

main :: IO ()
main = murv "http://localhost:8000/api" (const $ KeyPressed ' ') counter Nothing

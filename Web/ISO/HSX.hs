{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TypeFamilies, RankNTypes #-}
module Web.ISO.HSX where

import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHCJS.Marshal.Pure (pFromJSVal)
import GHCJS.Types (JSVal(..), JSString(..))
import Web.ISO.Types (Attr(Attr, Event), HTML(Element, CDATA), FormEvent(Change, Input), MouseEvent(Click), descendants)

default (Text)

{- HSX2HS -}

genElement (d, t) a c =
    let c' = (concat c)
    in Element t {- [] -} a Nothing (descendants c') c'

genEElement (d, t) a = genElement (d, t) a []

fromStringLit = pack

class AsChild action c where
    asChild :: c -> [HTML action]

instance AsChild action Text where
    asChild t = [CDATA True t]

instance AsChild action String where
    asChild t = [CDATA True (pack t)]

instance (parentAction ~ action) => AsChild parentAction (HTML action) where
    asChild t = [t]

instance (parentAction ~ action) => AsChild parentAction [HTML action] where
    asChild t = t

data KV k v = k := v

class AsAttr action a where
    asAttr :: a -> Attr action

instance AsAttr action (KV Text Text) where
    asAttr (k := v) = Attr k v

instance AsAttr action (KV Text action) where
    asAttr (type' := action) =
        case type' of
          "onchange" -> Event Change (const $ pure action)
          "onclick"  -> Event Click  (const $ pure action)
          "oninput"  -> Event Input  (const $ pure action)
--          "onblur"   -> Event Blur   (const $ pure action)
          _ -> error $ "unsupported event: " ++ (unpack type')
{-
instance AsAttr action (KV Text (JSVal -> action)) where
    asAttr (type' := action) =
        case type' of
          "onchange" -> Event Change (action . pFromJSVal)
--          "onclick"  -> Event Click  action
--          "oninput"  -> Event Input  action
--          "onblur"   -> Event Blur   action
          _ -> error $ "unsupported event: " ++ (unpack type')
-}
{-
instance AsAttr action (KV Text (IO String, (String -> action), action)) where
    asAttr (type' := action) =
        case type' of
          "onchange" -> Event (Change, action)
          "onclick"  -> Event (Click, action)
          "oninput"  -> Event (Input, action)
          _ -> error $ "unsupported event: " ++ (unpack type')
-}
instance (action' ~ action) => AsAttr action' (Attr action) where
    asAttr a = a

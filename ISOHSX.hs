{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TypeFamilies, RankNTypes #-}
module ISOHSX where

import Data.Text (Text, pack)
import Types

default (Text)

{- HSX2HS -}

genElement (d, t) a c = Element t [] a c

genEElement (d, t) a = genElement (d, t) a []

fromStringLit = pack

class AsChild action c where
    asChild :: c -> HTML action

instance AsChild action Text where
    asChild t = CDATA True t

instance AsChild action String where
    asChild t = CDATA True (pack t)

instance (parentAction ~ action) => AsChild parentAction (HTML action) where
    asChild t = t

data KV k v = k := v

class AsAttr action a where
    asAttr :: a -> Attr action

instance AsAttr action (KV Text Text) where
    asAttr (k := v) = Attr k v

instance AsAttr action (KV Text action) where
    asAttr (type' := action) =
        case type' of
          "onchange" -> Event (Change, action)
          "onclick"  -> Event (Click, action)

instance (action' ~ action) => AsAttr action' (Attr action) where
    asAttr a = a


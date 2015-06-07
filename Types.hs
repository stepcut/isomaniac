{-# LANGUAGE JavaScriptFFI #-}
module Types where

import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHCJS.Foreign (ToJSString(..), FromJSString(..), jsNull)
import GHCJS.Marshal (ToJSRef(..), FromJSRef(..))
import GHCJS.Types (JSRef(..), JSString(..), castRef, nullRef, isNull, isUndefined)

-- * JSNode

newtype JSNode = JSNode (JSRef JSNode) -- deriving (Eq)

unJSNode (JSNode o) = o

instance ToJSRef JSNode where
  toJSRef = return . unJSNode
  {-# INLINE toJSRef #-}

instance FromJSRef JSNode where
  fromJSRef = return . fmap JSNode . maybeJSNullOrUndefined
  {-# INLINE fromJSRef #-}

-- * IsJSNode

class IsJSNode obj where
    toJSNode :: (IsJSNode obj) => obj -> JSNode

instance IsJSNode JSNode where
    toJSNode = id

-- * JSNodeList

newtype JSNodeList = JSNodeList (JSRef JSNodeList) -- deriving (Eq)

unJSNodeList (JSNodeList o) = o

instance ToJSRef JSNodeList where
  toJSRef = return . unJSNodeList
  {-# INLINE toJSRef #-}

instance FromJSRef JSNodeList where
  fromJSRef = return . fmap JSNodeList . maybeJSNullOrUndefined
  {-# INLINE fromJSRef #-}

instance IsJSNode JSNodeList where
    toJSNode = JSNode . castRef . unJSNodeList


foreign import javascript unsafe "$1[\"item\"]($2)" js_item ::
        JSRef JSNodeList -> Word -> IO (JSRef JSNode)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NodeList.item Mozilla NodeList.item documentation> 
item ::
     (MonadIO m) => JSNodeList -> Word -> m (Maybe JSNode)
item self index
  = liftIO
      ((js_item (unJSNodeList self) index) >>= fromJSRef)

-- foreign import javascript unsafe "$1[\"length\"]" js_getLength ::
--         JSRef NodeList -> IO Word

-- * JSDocument

newtype JSDocument = JSDocument (JSRef JSDocument) -- deriving Eq

unJSDocument (JSDocument o) = o

instance ToJSRef JSDocument where
  toJSRef = return . unJSDocument
  {-# INLINE toJSRef #-}

instance FromJSRef JSDocument where
  fromJSRef = return . fmap JSDocument . maybeJSNullOrUndefined
  {-# INLINE fromJSRef #-}

maybeJSNullOrUndefined :: JSRef a -> Maybe (JSRef a)
maybeJSNullOrUndefined r | isNull r || isUndefined r = Nothing
maybeJSNullOrUndefined r = Just r

instance IsJSNode JSDocument where
    toJSNode = JSNode . castRef . unJSDocument

foreign import javascript unsafe "new window[\"Document\"]()"
        js_newDocument :: IO (JSRef JSDocument)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document Mozilla Document documentation>
newJSDocument :: (MonadIO m) => m (Maybe JSDocument)
newJSDocument = liftIO (js_newDocument >>= fromJSRef) -- fromJSRefUnchecked)

foreign import javascript unsafe "$r = document"
  ghcjs_currentDocument :: IO (JSRef JSDocument)

currentDocument :: IO (Maybe JSDocument)
currentDocument = fmap JSDocument . maybeJSNullOrUndefined <$> ghcjs_currentDocument

-- * JSElement

newtype JSElement = JSElement (JSRef JSElement) -- deriving (Eq)

unJSElement (JSElement o) = o

instance ToJSRef JSElement where
  toJSRef = return . unJSElement
  {-# INLINE toJSRef #-}

instance FromJSRef JSElement where
  fromJSRef = return . fmap JSElement . maybeJSNullOrUndefined
  {-# INLINE fromJSRef #-}

instance IsJSNode JSElement where
    toJSNode = JSNode . castRef . unJSElement

-- * createJSElement

foreign import javascript unsafe "$1[\"createElement\"]($2)"
        js_createJSElement ::
        JSRef JSDocument -> JSString -> IO (JSRef JSElement)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/JSDocument.createJSElement Mozilla JSDocument.createJSElement documentation> 
createJSElement ::
              (MonadIO m, ToJSString tagName) =>
                JSDocument -> tagName -> m (Maybe JSElement)
createJSElement document tagName
  = liftIO ((js_createJSElement (unJSDocument document) (toJSString tagName))
            >>= fromJSRef)

foreign import javascript unsafe "$1[\"getElementsByName\"]($2)"
        js_getElementsByName ::
        JSRef JSDocument -> JSString -> IO (JSRef JSNodeList)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByName Mozilla Document.getElementsByName documentation> 
getElementsByName ::
                  (MonadIO m, ToJSString elementName) =>
                    JSDocument -> elementName -> m (Maybe JSNodeList)
getElementsByName self elementName
  = liftIO
      ((js_getElementsByName (unJSDocument self)) (toJSString elementName)
       >>= fromJSRef)

foreign import javascript unsafe "$1[\"getElementsByTagName\"]($2)"
        js_getElementsByTagName ::
        JSRef JSDocument -> JSString -> IO (JSRef JSNodeList)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByTagName Mozilla Document.getElementsByTagName documentation> 
getElementsByTagName ::
                     (MonadIO m, ToJSString tagname) =>
                       JSDocument -> tagname -> m (Maybe JSNodeList)
getElementsByTagName self tagname
  = liftIO
      ((js_getElementsByTagName (unJSDocument self) (toJSString tagname))
       >>= fromJSRef)

-- * appendJSChild

foreign import javascript unsafe "$1[\"appendChild\"]($2)"
        js_appendChild :: JSRef JSNode -> JSRef JSNode -> IO (JSRef JSNode)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.appendChild Mozilla Node.appendChild documentation>

appendJSChild ::
            (MonadIO m, IsJSNode self, IsJSNode newChild) =>
              self -> Maybe newChild -> m (Maybe JSNode)
appendJSChild self newChild
  = liftIO
      ((js_appendChild (unJSNode (toJSNode self))
          (maybe jsNull (unJSNode . toJSNode) newChild))
         >>= fromJSRef)


-- * JSTextNode

newtype JSTextNode = JSTextNode (JSRef JSTextNode) -- deriving (Eq)

unJSTextNode (JSTextNode o) = o

instance ToJSRef JSTextNode where
  toJSRef = return . unJSTextNode
  {-# INLINE toJSRef #-}

instance FromJSRef JSTextNode where
  fromJSRef = return . fmap JSTextNode . maybeJSNullOrUndefined
  {-# INLINE fromJSRef #-}

instance IsJSNode JSTextNode where
    toJSNode = JSNode . castRef . unJSTextNode

-- * createTextNode

foreign import javascript unsafe "$1[\"createTextNode\"]($2)"
        js_createTextNode :: JSRef JSDocument -> JSString -> IO (JSRef JSTextNode)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.createTextNode Mozilla Document.createTextNode documentation> 
createJSTextNode ::
               (MonadIO m, ToJSString data') =>
                 JSDocument -> data' -> m (Maybe JSTextNode)
createJSTextNode document data'
  = liftIO
      ((js_createTextNode (unJSDocument document)
          (toJSString data'))
         >>= fromJSRef)

data Attr = Attr Text Text

data HTML
  = Element [Attr] [HTML]
  | CDATA Bool Text

renderHTML :: HTML -> IO ()
renderHTML html = return ()


{-# LANGUAGE JavaScriptFFI #-}
module Types where

import Control.Monad.Trans (MonadIO(..))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue)
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHCJS.Foreign (ToJSString(..), FromJSString(..), ForeignRetention(AlwaysRetain), asyncCallback, jsNull)
import GHCJS.Marshal (ToJSRef(..), FromJSRef(..))
import GHCJS.Types (JSRef(..), JSString(..), JSFun, castRef, nullRef, isNull, isUndefined)

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

foreign import javascript unsafe "$1[\"removeChild\"]($2)"
        js_removeChild :: JSRef JSNode -> JSRef JSNode -> IO (JSRef JSNode)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.removeChild Mozilla Node.removeChild documentation>
removeChild ::
            (MonadIO m, IsJSNode self, IsJSNode oldChild) =>
              self -> Maybe oldChild -> m (Maybe JSNode)
removeChild self oldChild
  = liftIO
      ((js_removeChild (unJSNode (toJSNode self))
          (maybe jsNull (unJSNode . toJSNode) oldChild))
         >>= fromJSRef)

foreign import javascript unsafe "$1[\"firstChild\"]"
        js_getFirstChild :: JSRef JSNode -> IO (JSRef JSNode)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.firstChild Mozilla Node.firstChild documentation>
getFirstChild :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
getFirstChild self
  = liftIO ((js_getFirstChild (unJSNode (toJSNode self))) >>= fromJSRef)

removeChildren
    :: (MonadIO m, IsJSNode self) =>
       self
    -> m ()
removeChildren self =
    do mc <- getFirstChild self
       case mc of
         Nothing -> return ()
         (Just _) ->
             do removeChild self mc
                removeChildren self
{-
foreign import javascript unsafe "$1[\"setAttribute\"]($2, $3)"
        js_setAttribute :: JSRef Element -> JSString -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
setAttribute ::
             (MonadIO m, ToJSString name, ToJSString value) =>
               JSElement -> name -> value -> m ()
setAttribute self name value
  = liftIO
      (js_setAttribute (unElement self) (toJSString name) (toJSString value))
-}
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

-- * Events

-- FIXME: Element is overly restrictive
foreign import javascript unsafe "$1[\"addEventListener\"]($2, $3,\n$4)"
   js_addEventListener ::
       JSRef JSElement -> JSString -> JSFun (IO ()) -> Bool -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.addEventListener Mozilla EventTarget.addEventListener documentation>
addEventListener ::
                 (MonadIO m, ToJSString type') =>
                   JSElement -> type' -> JSFun (IO ()) -> Bool -> m ()
addEventListener self type' listener useCapture
  = liftIO
      (js_addEventListener (unJSElement self)
         (toJSString type')
         listener
--         (maybe jsNull pToJSRef listener)
         useCapture)

-- * Pure HTML

data EventType
    = Change
    | Click

data Attr action
    = Attr Text Text
    | Event (EventType, action)

instance ToJSString EventType where
    toJSString Change = toJSString "change"
    toJSString Click  = toJSString "click"

data HTML action
  = Element Text [(EventType, action)] [Attr action] [HTML action]
  | CDATA Bool Text

renderHTML :: (MonadIO m) => (action -> IO ()) -> JSDocument -> HTML action -> m (Maybe JSNode)
renderHTML _ doc (CDATA _ t) = fmap (fmap toJSNode) $ createJSTextNode doc t
renderHTML handle doc (Element tag events attrs children) =
    do me <- createJSElement doc tag
       case me of
         Nothing -> return Nothing
         (Just e) ->
             do mapM_ (\c -> appendJSChild e =<< renderHTML handle doc c) children
                let events' = [ ev | Event ev <- attrs]
                liftIO $ mapM_ (handleEvent e) events'
                return (Just $ toJSNode e)
    where
--       handleEvent :: JSElement -> (EventType, action) -> IO ()
      handleEvent e (et, action) =
          do cb <- asyncCallback AlwaysRetain (handle action) -- FIXME: free ?
             addEventListener e et cb False




data MUV  model action = MUV
    { model  :: model
    , update :: action -> model -> model
    , view   :: model  -> HTML action
    }

mainLoop :: JSDocument -> JSNode -> MUV model action -> IO ()
mainLoop document body (MUV model update view) =
    do queue <- atomically newTQueue
       html <- renderHTML (handleAction queue) document (view model)
       removeChildren body
       appendJSChild body html
       loop queue model
    where
      handleAction queue = \action -> atomically $ writeTQueue queue action
      loop queue model =
          do action <- atomically $ readTQueue queue
             let model' = update action model
             html <- renderHTML (handleAction queue) document (view model')
             removeChildren body
             appendJSChild body html
             loop queue model'

muv :: MUV model action -> IO ()
muv m =
    do (Just document) <- currentDocument
       (Just bodyList) <- getElementsByTagName document "body"
       (Just body)     <- item bodyList 0
       mainLoop document body m


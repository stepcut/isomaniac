{-# LANGUAGE JavaScriptFFI, ExistentialQuantification, ScopedTypeVariables  #-}
module Web.ISO.Types where

import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (fromJust)
import Data.Aeson.Types (Parser, Result(..), parse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import GHCJS.Foreign (ToJSString(..), FromJSString(..), ForeignRetention(AlwaysRetain), asyncCallback, jsNull)
import GHCJS.Marshal (ToJSRef(..), FromJSRef(..))
import GHCJS.Types (JSRef(..), JSString(..), JSFun, castRef, nullRef, isNull, isUndefined)

instance Eq (JSRef a) where
  a == b = js_eq a b

foreign import javascript unsafe
  "$1===$2" js_eq :: JSRef a -> JSRef a -> Bool

fromJSRefUnchecked :: (FromJSRef a) => JSRef a -> IO a
fromJSRefUnchecked j =
    do x <- fromJSRef j
       case x of
         Nothing -> error "failed."
         (Just a) -> return a

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

foreign import javascript unsafe "$1[\"length\"]" js_length ::
        JSRef JSNode -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NodeList.item Mozilla NodeList.item documentation>
getLength :: (MonadIO m, IsJSNode self) => self -> m Word
getLength self
  = liftIO (js_length (unJSNode (toJSNode self))) -- >>= fromJSRefUnchecked)

-- foreign import javascript unsafe "$1[\"length\"]" js_getLength ::
--         JSRef NodeList -> IO Word


-- * parentNode

foreign import javascript unsafe "$1[\"parentNode\"]"
        js_parentNode :: JSRef JSNode -> IO (JSRef JSNode)

parentNode :: (MonadIO m, IsJSNode self ) => self -> m (Maybe JSNode)
parentNode self =
    liftIO (js_parentNode (unJSNode (toJSNode self)) >>= fromJSRef)

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
newJSDocument :: (MonadIO m) => m JSDocument
newJSDocument = liftIO (js_newDocument >>= fromJSRefUnchecked)

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

-- * childNodes

foreign import javascript unsafe "$1[\"childNodes\"]"
        js_childNodes :: JSRef JSNode -> IO (JSRef JSNodeList)

childNodes :: (MonadIO m, IsJSNode self) => self -> m JSNodeList
childNodes self
    = liftIO ((js_childNodes (unJSNode (toJSNode self))) >>= fromJSRefUnchecked)

-- * getElementsByName

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

foreign import javascript unsafe "$1[\"getElementById\"]($2)"
        js_getElementsById ::
        JSRef JSDocument -> JSString -> IO (JSRef JSElement)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByTagName Mozilla Document.getElementsById documentation>
getElementById ::
                     (MonadIO m, ToJSString tagname) =>
                       JSDocument -> tagname -> m (Maybe JSElement)
getElementById self ident
  = liftIO
      ((js_getElementsById (unJSDocument self) (toJSString ident))
       >>= fromJSRef)

-- * appendChild

foreign import javascript unsafe "$1[\"appendChild\"]($2)"
        js_appendChild :: JSRef JSNode -> JSRef JSNode -> IO (JSRef JSNode)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.appendChild Mozilla Node.appendChild documentation>

appendChild ::
            (MonadIO m, IsJSNode self, IsJSNode newChild) =>
              self -> Maybe newChild -> m (Maybe JSNode)
appendChild self newChild
  = liftIO
      ((js_appendChild (unJSNode (toJSNode self))
          (maybe jsNull (unJSNode . toJSNode) newChild))
         >>= fromJSRef)
{-
probably broken on IE9

-- * textContent

foreign import javascript unsafe "$1[\"textContent\"] = $2"
        js_setTextContent :: JSRef JSNode -> JSString -> IO ()

setTextContent :: (MonadIO m, IsJSNode self, ToJSString content) =>
                  self
               -> content
               -> m ()
setTextContent self content =
    liftIO $ (js_setTextContent (unJSNode (toJSNode self)) (toJSString content))
-}

-- * replaceData

-- FIMXE: perhaps only a TextNode?
foreign import javascript unsafe "$1[\"replaceData\"]($2, $3, $4)" js_replaceData
    :: JSRef JSNode
    -> Word
    -> Word
    -> JSString
    -> IO ()

replaceData :: (MonadIO m, IsJSNode self, ToJSString string) =>
               self
            -> Word
            -> Word
            -> string
            -> m ()
replaceData self start length string =
    liftIO (js_replaceData (unJSNode (toJSNode self)) start length (toJSString string))

-- * removeChild

foreign import javascript unsafe "$1[\"removeChild\"]($2)"
        js_removeChild :: JSRef JSNode -> JSRef JSNode -> IO (JSRef JSNode)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.removeChild Mozilla Node.removeChild documentation>
removeChild ::  -- FIMXE: really a maybe?
            (MonadIO m, IsJSNode self, IsJSNode oldChild) =>
              self -> Maybe oldChild -> m (Maybe JSNode)
removeChild self oldChild
  = liftIO
      ((js_removeChild (unJSNode (toJSNode self))
          (maybe jsNull (unJSNode . toJSNode) oldChild))
         >>= fromJSRef)

-- * replaceChild

foreign import javascript unsafe "$1[\"replaceChild\"]($2, $3)"
        js_replaceChild :: JSRef JSNode -> JSRef JSNode -> JSRef JSNode -> IO (JSRef JSNode)

replaceChild ::
            (MonadIO m, IsJSNode self, IsJSNode newChild, IsJSNode oldChild) =>
              self -> newChild -> oldChild -> m (Maybe JSNode)
replaceChild self newChild oldChild
  = liftIO
      (js_replaceChild (unJSNode (toJSNode self))
                       ((unJSNode . toJSNode) newChild)
                       ((unJSNode . toJSNode) oldChild)
         >>= fromJSRef)

-- * firstChild

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

foreign import javascript unsafe "$1[\"setAttribute\"]($2, $3)"
        js_setAttribute :: JSRef JSElement -> JSString -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
setAttribute ::
             (MonadIO m, ToJSString name, ToJSString value) =>
               JSElement -> name -> value -> m ()
setAttribute self name value
  = liftIO
      (js_setAttribute (unJSElement self) (toJSString name) (toJSString value))

-- * value

foreign import javascript unsafe "$1[\"value\"]"
        js_getValue :: JSRef JSNode -> IO (JSRef JSString)

getValue :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSString)
getValue self
  = liftIO ((js_getValue (unJSNode (toJSNode self))) >>= fromJSRef)

foreign import javascript unsafe "$1[\"value\"] = $2"
        js_setValue :: JSRef JSNode -> JSString -> IO ()

setValue :: (MonadIO m, IsJSNode self, ToJSString str) => self -> str -> m ()
setValue self str =
    liftIO (js_setValue (unJSNode (toJSNode self)) (toJSString str))

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

newtype EventTarget = EventTarget { unEventTarget :: JSRef EventTarget }

instance Eq (EventTarget) where
  (EventTarget a) == (EventTarget b) = js_eq a b

instance ToJSRef EventTarget where
  toJSRef = return . unEventTarget
  {-# INLINE toJSRef #-}

instance FromJSRef EventTarget where
  fromJSRef = return . fmap EventTarget . maybeJSNullOrUndefined
  {-# INLINE fromJSRef #-}

class IsEventTarget o where
    toEventTarget :: o -> EventTarget
--    toEventTarget = EventTarget

instance IsEventTarget JSElement where
    toEventTarget = EventTarget . castRef . unJSElement

-- FIXME: Element is overly restrictive
foreign import javascript unsafe "$1[\"addEventListener\"]($2, $3,\n$4)"
   js_addEventListener ::
       JSRef EventTarget -> JSString -> JSFun (IO ()) -> Bool -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.addEventListener Mozilla EventTarget.addEventListener documentation>
addEventListener ::
                 (MonadIO m, IsEventTarget self, ToJSString type') =>
                   self -> type' -> JSFun (IO ()) -> Bool -> m ()
addEventListener self type' listener useCapture
  = liftIO
      (js_addEventListener (unEventTarget (toEventTarget self))
         (toJSString type')
         listener
--         (maybe jsNull pToJSRef listener)
         useCapture)

-- * XMLHttpRequest
newtype XMLHttpRequest = XMLHttpRequest { unXMLHttpRequest :: JSRef XMLHttpRequest }

instance Eq (XMLHttpRequest) where
  (XMLHttpRequest a) == (XMLHttpRequest b) = js_eq a b

instance IsEventTarget XMLHttpRequest where
    toEventTarget = EventTarget . castRef . unXMLHttpRequest

{-
instance PToJSRef XMLHttpRequest where
  pToJSRef = unXMLHttpRequest
  {-# INLINE pToJSRef #-}

instance PFromJSRef XMLHttpRequest where
  pFromJSRef = XMLHttpRequest
  {-# INLINE pFromJSRef #-}
-}
instance ToJSRef XMLHttpRequest where
  toJSRef = return . unXMLHttpRequest
  {-# INLINE toJSRef #-}

instance FromJSRef XMLHttpRequest where
  fromJSRef = return . fmap XMLHttpRequest . maybeJSNullOrUndefined
  {-# INLINE fromJSRef #-}

foreign import javascript unsafe "new window[\"XMLHttpRequest\"]()"
        js_newXMLHttpRequest :: IO (JSRef XMLHttpRequest)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest Mozilla XMLHttpRequest documentation>
newXMLHttpRequest :: (MonadIO m) => m XMLHttpRequest
newXMLHttpRequest
  = liftIO (js_newXMLHttpRequest >>= fromJSRefUnchecked)

foreign import javascript unsafe "$1[\"open\"]($2, $3, $4)"
        js_open ::
        JSRef XMLHttpRequest ->
          JSString -> JSString -> Bool -> {- JSString -> JSString -> -} IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.open Mozilla XMLHttpRequest.open documentation>
open ::
     (MonadIO m, ToJSString method, ToJSString url) =>
       XMLHttpRequest -> method -> url -> Bool -> m ()
open self method url async
  = liftIO
      (js_open (unXMLHttpRequest self) (toJSString method)
         (toJSString url)
         async)

-- foreign import javascript interruptible "h$dom$sendXHR($1, $2, $c);" js_send :: JSRef XMLHttpRequest -> JSRef () -> IO Int
{-
-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
send :: (MonadIO m) => XMLHttpRequest -> m ()
send self = liftIO $ js_send (unXMLHttpRequest self) jsNull >> return () -- >>= throwXHRError
-}

foreign import javascript unsafe "$1[\"send\"]($2)" js_sendString ::
        JSRef XMLHttpRequest -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
sendString :: (MonadIO m, ToJSRef str, ToJSString str) => XMLHttpRequest -> str -> m ()
sendString self str =
    liftIO $ do r <- toJSRef str
                js_sendString (unXMLHttpRequest self) (castRef r) >> return () -- >>= throwXHRError

foreign import javascript unsafe "$1[\"readyState\"]"
        js_getReadyState :: JSRef XMLHttpRequest -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.readyState Mozilla XMLHttpRequest.readyState documentation>
getReadyState :: (MonadIO m) => XMLHttpRequest -> m Word
getReadyState self
  = liftIO (js_getReadyState (unXMLHttpRequest self))

foreign import javascript unsafe "$1[\"responseType\"]"
        js_getResponseType ::
        JSRef XMLHttpRequest -> IO (JSRef XMLHttpRequestResponseType)

-- | <Https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseType Mozilla XMLHttpRequest.responseType documentation>
getResponseType ::
                (MonadIO m) => XMLHttpRequest -> m XMLHttpRequestResponseType
getResponseType self
  = liftIO
      ((js_getResponseType (unXMLHttpRequest self)) >>=
         fromJSRefUnchecked)

data XMLHttpRequestResponseType = XMLHttpRequestResponseType
                                | XMLHttpRequestResponseTypeArraybuffer
                                | XMLHttpRequestResponseTypeBlob
                                | XMLHttpRequestResponseTypeDocument
                                | XMLHttpRequestResponseTypeJson
                                | XMLHttpRequestResponseTypeText
foreign import javascript unsafe "\"\""
        js_XMLHttpRequestResponseType :: JSRef XMLHttpRequestResponseType

foreign import javascript unsafe "\"arraybuffer\""
        js_XMLHttpRequestResponseTypeArraybuffer ::
        JSRef XMLHttpRequestResponseType

foreign import javascript unsafe "\"blob\""
        js_XMLHttpRequestResponseTypeBlob ::
        JSRef XMLHttpRequestResponseType

foreign import javascript unsafe "\"document\""
        js_XMLHttpRequestResponseTypeDocument ::
        JSRef XMLHttpRequestResponseType

foreign import javascript unsafe "\"json\""
        js_XMLHttpRequestResponseTypeJson ::
        JSRef XMLHttpRequestResponseType

foreign import javascript unsafe "\"text\""
        js_XMLHttpRequestResponseTypeText ::
        JSRef XMLHttpRequestResponseType

{-
instance PToJSRef XMLHttpRequestResponseType where
        pToJSRef XMLHttpRequestResponseType = js_XMLHttpRequestResponseType
        pToJSRef XMLHttpRequestResponseTypeArraybuffer
          = js_XMLHttpRequestResponseTypeArraybuffer
        pToJSRef XMLHttpRequestResponseTypeBlob
          = js_XMLHttpRequestResponseTypeBlob
        pToJSRef XMLHttpRequestResponseTypeDocument
          = js_XMLHttpRequestResponseTypeDocument
        pToJSRef XMLHttpRequestResponseTypeJson
          = js_XMLHttpRequestResponseTypeJson
        pToJSRef XMLHttpRequestResponseTypeText
          = js_XMLHttpRequestResponseTypeText
-}
instance ToJSRef XMLHttpRequestResponseType where
        toJSRef XMLHttpRequestResponseType
          = return js_XMLHttpRequestResponseType
        toJSRef XMLHttpRequestResponseTypeArraybuffer
          = return js_XMLHttpRequestResponseTypeArraybuffer
        toJSRef XMLHttpRequestResponseTypeBlob
          = return js_XMLHttpRequestResponseTypeBlob
        toJSRef XMLHttpRequestResponseTypeDocument
          = return js_XMLHttpRequestResponseTypeDocument
        toJSRef XMLHttpRequestResponseTypeJson
          = return js_XMLHttpRequestResponseTypeJson
        toJSRef XMLHttpRequestResponseTypeText
          = return js_XMLHttpRequestResponseTypeText

{-
instance PFromJSRef XMLHttpRequestResponseType where
        pFromJSRef x
          | x == js_XMLHttpRequestResponseType = XMLHttpRequestResponseType
        pFromJSRef x
          | x == js_XMLHttpRequestResponseTypeArraybuffer =
            XMLHttpRequestResponseTypeArraybuffer
        pFromJSRef x
          | x == js_XMLHttpRequestResponseTypeBlob =
            XMLHttpRequestResponseTypeBlob
        pFromJSRef x
          | x == js_XMLHttpRequestResponseTypeDocument =
            XMLHttpRequestResponseTypeDocument
        pFromJSRef x
          | x == js_XMLHttpRequestResponseTypeJson =
            XMLHttpRequestResponseTypeJson
        pFromJSRef x
          | x == js_XMLHttpRequestResponseTypeText =
            XMLHttpRequestResponseTypeText
-}
instance FromJSRef XMLHttpRequestResponseType where
--        fromJSRefUnchecked = return . pFromJSRef
        fromJSRef x
            | x == js_XMLHttpRequestResponseType =
                return (Just XMLHttpRequestResponseType)
            | x == js_XMLHttpRequestResponseTypeArraybuffer =
                return (Just XMLHttpRequestResponseTypeArraybuffer)
            | x == js_XMLHttpRequestResponseTypeBlob =
                return (Just XMLHttpRequestResponseTypeBlob)
            | x == js_XMLHttpRequestResponseTypeDocument =
                return (Just XMLHttpRequestResponseTypeDocument)
            | x == js_XMLHttpRequestResponseTypeJson =
                return (Just XMLHttpRequestResponseTypeJson)
            | x == js_XMLHttpRequestResponseTypeText =
                return (Just XMLHttpRequestResponseTypeText)
{-
foreign import javascript unsafe "$1[\"response\"]" js_getResponse
        :: JSRef XMLHttpRequest -> IO (JSRef GObject)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.response Mozilla XMLHttpRequest.response documentation>
getResponse :: (MonadIO m) => XMLHttpRequest -> m (Maybe GObject)
getResponse self
  = liftIO ((js_getResponse (unXMLHttpRequest self)) >>= fromJSRef)
-}

foreign import javascript unsafe "$1[\"responseText\"]"
        js_getResponseText :: JSRef XMLHttpRequest -> IO JSString

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseText Mozilla XMLHttpRequest.responseText documentation> 
getResponseText ::
                (MonadIO m, FromJSString result) => XMLHttpRequest -> m result
getResponseText self
  = liftIO
      (fromJSString <$> (js_getResponseText (unXMLHttpRequest self)))

foreign import javascript unsafe "$1[\"status\"]" js_getStatus ::
        JSRef XMLHttpRequest -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.status Mozilla XMLHttpRequest.status documentation>
getStatus :: (MonadIO m) => XMLHttpRequest -> m Word
getStatus self = liftIO (js_getStatus (unXMLHttpRequest self))

foreign import javascript unsafe "$1[\"statusText\"]"
        js_getStatusText :: JSRef XMLHttpRequest -> IO JSString

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.statusText Mozilla XMLHttpRequest.statusText documentation>
getStatusText ::
              (MonadIO m, FromJSString result) => XMLHttpRequest -> m result
getStatusText self
  = liftIO
      (fromJSString <$> (js_getStatusText (unXMLHttpRequest self)))

foreign import javascript unsafe "$1[\"responseURL\"]"
        js_getResponseURL :: JSRef XMLHttpRequest -> IO JSString


-- * Pure HTML

data EventType
    = Change
    | Click
    | Input
    | Blur

data Attr action
    = Attr Text Text
    | Event (EventType, Maybe JSString -> action)

instance ToJSString EventType where
    toJSString Change = toJSString "change"
    toJSString Click  = toJSString "click"
    toJSString Input  = toJSString "input"
    toJSString Blur   = toJSString "blur"

data HTML action
  = forall a. Element Text {- [(EventType, Parser a, a -> action)] -} [Attr action] Int [HTML action]
  | CDATA Bool Text
--   | Children [HTML action]

instance Show (Attr action) where
    show (Attr k v) = (Text.unpack k) <> " := " <> (Text.unpack v) <> " "
    show (Event eventType) = "Event "

instance Show (HTML action) where
    show (Element tagName attrs count children) =
        (Text.unpack tagName) <> " [" <> concat (map show attrs) <> "]\n" <> concat (map showChild children)
        where
          showChild c = "    " <> show c <> "\n"
    show (CDATA b txt) = Text.unpack txt

descendants :: [HTML action] -> Int
descendants elems = sum [ d | Element _ _ d _ <- elems] + (length elems)

{-
flattenHTML :: HTML action -> HTML action
flattenHTML h@(CDATA _ _) = h
flattenHTML h@(Children _) = h
flattenHTML (Element t acts attrs children)
-}
renderHTML :: forall action m. (MonadIO m) => (action -> IO ()) -> JSDocument -> HTML action -> m (Maybe JSNode)
renderHTML _ doc (CDATA _ t) = fmap (fmap toJSNode) $ createJSTextNode doc t
renderHTML handle doc (Element tag {- events -} attrs _ children) =
    do me <- createJSElement doc tag
       case me of
         Nothing -> return Nothing
         (Just e) ->
             do mapM_ (\c -> appendChild e =<< renderHTML handle doc c) children
                let events' = [ ev | Event ev <- attrs]
                    attrs'  = [ (k,v) | Attr k v <- attrs]
                liftIO $ mapM_ (\(k, v) -> setAttribute e k v) attrs'
                liftIO $ mapM_ (handleEvent e) events'
                return (Just $ toJSNode e)
    where
      handle' :: JSElement -> (Maybe JSString -> action) -> IO ()
      handle' elem toAction =
          do ms <- getValue elem
             handle (toAction ms)
      handleEvent :: JSElement -> (EventType, Maybe JSString -> action) -> IO ()
      handleEvent elem (eventType, toAction) =
          do cb <- asyncCallback AlwaysRetain (handle' elem toAction) -- FIXME: free ?
             addEventListener elem eventType cb False


{-
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
-}
{-
data MURV  model action remote = MURV
    { model  :: model
    , update :: action -> model -> (model, Maybe remote)
    , view   :: model  -> HTML action
    }

mainLoopRemote :: (ToJSString remote) => Text -> (Text -> action) -> JSDocument -> JSNode -> MURV model action remote -> IO ()
mainLoopRemote url h document body (MURV model update view) =
    do queue <- atomically newTQueue
       html <- renderHTML (handleAction queue) document (view model)
       removeChildren body
       appendJSChild body html
       xhr <- newXMLHttpRequest
       cb <- asyncCallback AlwaysRetain (handleXHR queue xhr)
       addEventListener xhr "readystatechange" cb False
--       remoteLoop queue xhr
       loop xhr queue model
    where
      handleXHR queue xhr =
          do t <- getResponseText xhr
             atomically $ writeTQueue queue (h t)
      handleAction queue = \action -> atomically $ writeTQueue queue action
--      remoteLoop queue xhr = forkIO $
--          return ()
      loop xhr queue model =
          do action <- atomically $ readTQueue queue
             let (model', mremote') = update action model
             html <- renderHTML (handleAction queue) document (view model')
             removeChildren body
             appendJSChild body html
             case mremote' of
               Nothing -> return ()
               (Just remote) ->
                   do open xhr "POST" url True
                      sendString xhr (toJSString remote)
             loop xhr queue model'

murv :: (ToJSString remote) => Text -> (Text -> action) -> MURV model action remote -> IO ()
murv url h m =
    do (Just document) <- currentDocument
       murv <- do mmurv <- getElementById document "murv"
                  case mmurv of
                    (Just murv) -> return (toJSNode murv)
                    Nothing ->
                        do (Just bodyList) <- getElementsByTagName document "body"
                           (Just body)     <- item bodyList 0
                           return body
       mainLoopRemote url h document murv m
-}

{-# LANGUAGE JavaScriptFFI, ExistentialQuantification, ScopedTypeVariables, TemplateHaskell  #-}
module Web.ISO.Types where

import Control.Monad.Trans (MonadIO(..))
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.Maybe (fromJust)
import Data.Aeson.Types (Parser, Result(..), parse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import qualified Data.Text as Text
-- import GHCJS.Prim (ToJSString(..), FromJSString(..))
-- import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import GHCJS.Buffer
import GHCJS.Foreign (jsNull)
import GHCJS.Foreign.Callback (Callback, asyncCallback)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal))
import GHCJS.Types (JSVal(..), JSString(..),  nullRef, isNull, isUndefined)

instance Eq JSVal where
  a == b = js_eq a b

foreign import javascript unsafe
  "$1===$2" js_eq :: JSVal  -> JSVal  -> Bool


maybeJSNullOrUndefined :: JSVal -> Maybe JSVal
maybeJSNullOrUndefined r | isNull r || isUndefined r = Nothing
maybeJSNullOrUndefined r = Just r

{-
fromJSValUnchecked :: (FromJSVal a) => JSVal a -> IO a
fromJSValUnchecked j =
    do x <- fromJSVal j
       case x of
         Nothing -> error "failed."
         (Just a) -> return a
-}
-- * JSNode

newtype JSNode = JSNode JSVal

unJSNode (JSNode o) = o

instance ToJSVal JSNode where
  toJSVal = toJSVal . unJSNode
  {-# INLINE toJSVal #-}

instance FromJSVal JSNode where
  fromJSVal = return . fmap JSNode . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

-- * IsJSNode

class IsJSNode obj where
    toJSNode :: (IsJSNode obj) => obj -> JSNode

instance IsJSNode JSNode where
    toJSNode = id

-- * JSNodeList

newtype JSNodeList = JSNodeList JSVal

unJSNodeList (JSNodeList o) = o

instance ToJSVal JSNodeList where
  toJSVal = return . unJSNodeList
  {-# INLINE toJSVal #-}

instance FromJSVal JSNodeList where
  fromJSVal = return . fmap JSNodeList . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsJSNode JSNodeList where
    toJSNode = JSNode . unJSNodeList

foreign import javascript unsafe "$1[\"item\"]($2)" js_item ::
        JSNodeList -> Word -> IO JSNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NodeList.item Mozilla NodeList.item documentation>
item ::
     (MonadIO m) => JSNodeList -> Word -> m (Maybe JSNode)
item self index
  = liftIO
      ((js_item (self) index) >>= return . Just)

foreign import javascript unsafe "$1[\"length\"]" js_length ::
        JSNode -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NodeList.item Mozilla NodeList.item documentation>
getLength :: (MonadIO m, IsJSNode self) => self -> m Word
getLength self
  = liftIO (js_length ( (toJSNode self))) -- >>= fromJSValUnchecked)

-- foreign import javascript unsafe "$1[\"length\"]" js_getLength ::
--         JSVal NodeList -> IO Word


-- * parentNode

foreign import javascript unsafe "$1[\"parentNode\"]"
        js_parentNode :: JSNode -> IO JSVal

parentNode :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
parentNode self =
    liftIO (fromJSVal =<< js_parentNode (toJSNode self))

-- * JSDocument

newtype JSDocument = JSDocument JSVal

unJSDocument (JSDocument o) = o

instance ToJSVal JSDocument where
  toJSVal = return . unJSDocument
  {-# INLINE toJSVal #-}

instance FromJSVal JSDocument where
  fromJSVal = return . fmap JSDocument . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsJSNode JSDocument where
    toJSNode = JSNode . unJSDocument

foreign import javascript unsafe "new window[\"Document\"]()"
        js_newDocument :: IO JSDocument

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document Mozilla Document documentation>
newJSDocument :: (MonadIO m) => m JSDocument
newJSDocument = liftIO js_newDocument

foreign import javascript unsafe "$r = document"
  ghcjs_currentDocument :: IO JSDocument

currentDocument :: IO (Maybe JSDocument)
currentDocument = Just <$> ghcjs_currentDocument

-- * JSElement

newtype JSElement = JSElement JSVal

unJSElement (JSElement o) = o

instance ToJSVal JSElement where
  toJSVal = return . unJSElement
  {-# INLINE toJSVal #-}

instance FromJSVal JSElement where
  fromJSVal = return . fmap JSElement . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsJSNode JSElement where
    toJSNode = JSNode . unJSElement

-- * createJSElement

foreign import javascript unsafe "$1[\"createElement\"]($2)"
        js_createJSElement ::
        JSDocument -> JSString -> IO JSElement

-- | <https://developer.mozilla.org/en-US/docs/Web/API/JSDocument.createJSElement Mozilla JSDocument.createJSElement documentation>
createJSElement ::
              (MonadIO m) =>
                JSDocument -> Text -> m (Maybe JSElement)
createJSElement document tagName
  = liftIO ((js_createJSElement document (textToJSString tagName))
            >>= return . Just)

-- * childNodes

foreign import javascript unsafe "$1[\"childNodes\"]"
        js_childNodes :: JSNode -> IO JSNodeList

childNodes :: (MonadIO m, IsJSNode self) => self -> m JSNodeList
childNodes self
    = liftIO (js_childNodes (toJSNode self))

-- * getElementsByName

foreign import javascript unsafe "$1[\"getElementsByName\"]($2)"
        js_getElementsByName ::
        JSDocument -> JSString -> IO JSNodeList

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByName Mozilla Document.getElementsByName documentation>
getElementsByName ::
                  (MonadIO m) =>
                    JSDocument -> JSString -> m (Maybe JSNodeList)
getElementsByName self elementName
  = liftIO
      ((js_getElementsByName self) elementName
       >>= return . Just)

foreign import javascript unsafe "$1[\"getElementsByTagName\"]($2)"
        js_getElementsByTagName ::
        JSDocument -> JSString -> IO JSNodeList

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByTagName Mozilla Document.getElementsByTagName documentation>
getElementsByTagName ::
                     (MonadIO m) =>
                       JSDocument -> JSString -> m (Maybe JSNodeList)
getElementsByTagName self tagname
  = liftIO
      ((js_getElementsByTagName self tagname)
       >>= return . Just)

foreign import javascript unsafe "$1[\"getElementById\"]($2)"
        js_getElementsById ::
        JSDocument -> JSString -> IO JSElement

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByTagName Mozilla Document.getElementsById documentation>
getElementById ::
                     (MonadIO m) =>
                       JSDocument -> JSString -> m (Maybe JSElement)
getElementById self ident
  = liftIO
      ((js_getElementsById self ident)
       >>= return . Just)

-- * appendChild

foreign import javascript unsafe "$1[\"appendChild\"]($2)"
        js_appendChild :: JSNode -> JSNode -> IO JSNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.appendChild Mozilla Node.appendChild documentation>

appendChild :: (MonadIO m, IsJSNode self, IsJSNode newChild) =>
               self
            -> Maybe newChild
            -> m (Maybe JSNode)
appendChild self newChild
  = liftIO
      ((js_appendChild ( (toJSNode self))
          (maybe (JSNode jsNull) ( toJSNode) newChild))
         >>= return . Just)

{-
probably broken on IE9

-- * textContent

foreign import javascript unsafe "$1[\"textContent\"] = $2"
        js_setTextContent :: JSVal JSNode -> JSString -> IO ()

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
    :: JSNode
    -> Word
    -> Word
    -> JSString
    -> IO ()

replaceData :: (MonadIO m, IsJSNode self) =>
               self
            -> Word
            -> Word
            -> Text
            -> m ()
replaceData self start length string =
    liftIO (js_replaceData (toJSNode self) start length (textToJSString string))

-- * removeChild

foreign import javascript unsafe "$1[\"removeChild\"]($2)"
        js_removeChild :: JSNode -> JSNode -> IO JSNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.removeChild Mozilla Node.removeChild documentation>
removeChild ::  -- FIMXE: really a maybe?
            (MonadIO m, IsJSNode self, IsJSNode oldChild) =>
              self -> Maybe oldChild -> m (Maybe JSNode)
removeChild self oldChild
  = liftIO
      ((js_removeChild (toJSNode self)
          (maybe (JSNode jsNull) toJSNode oldChild))
         >>= return . Just)

-- * replaceChild

foreign import javascript unsafe "$1[\"replaceChild\"]($2, $3)"
        js_replaceChild :: JSNode -> JSNode -> JSNode -> IO JSNode

replaceChild ::
            (MonadIO m, IsJSNode self, IsJSNode newChild, IsJSNode oldChild) =>
              self -> newChild -> oldChild -> m (Maybe JSNode)
replaceChild self newChild oldChild
  = liftIO
      (js_replaceChild ((toJSNode self))
                       ((toJSNode) newChild)
                       ((toJSNode) oldChild)
         >>= return . Just)

-- * firstChild

foreign import javascript unsafe "$1[\"firstChild\"]"
        js_getFirstChild :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.firstChild Mozilla Node.firstChild documentation>
getFirstChild :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
getFirstChild self
  = liftIO ((js_getFirstChild ((toJSNode self))) >>= fromJSVal)

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
        js_setAttribute :: JSElement -> JSString -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
setAttribute ::
             (MonadIO m) =>
               JSElement -> Text -> Text -> m ()
setAttribute self name value
  = liftIO
      (js_setAttribute self (textToJSString name) (textToJSString value))

foreign import javascript unsafe "$1[\"style\"][\"$2\"] = $3"
        setStyle :: JSElement -> JSString -> JSString -> IO ()
{-
setCSS :: (MonadIO m) =>
          JSElement
       -> JSString
       -> JSString
       -> m ()
setCSS elem name value =
  liftIO $ js_setCSS elem name value
-}
-- * value

foreign import javascript unsafe "$1[\"value\"]"
        js_getValue :: JSNode -> IO JSString

getValue :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSString)
getValue self
  = liftIO ((js_getValue (toJSNode self)) >>= return . Just)

foreign import javascript unsafe "$1[\"value\"] = $2"
        js_setValue :: JSNode -> JSString -> IO ()

setValue :: (MonadIO m, IsJSNode self) => self -> Text -> m ()
setValue self str =
    liftIO (js_setValue (toJSNode self) (textToJSString str))

-- * JSTextNode

newtype JSTextNode = JSTextNode JSVal -- deriving (Eq)

unJSTextNode (JSTextNode o) = o

instance ToJSVal JSTextNode where
  toJSVal = return . unJSTextNode
  {-# INLINE toJSVal #-}

instance FromJSVal JSTextNode where
  fromJSVal = return . fmap JSTextNode . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsJSNode JSTextNode where
    toJSNode = JSNode . unJSTextNode

-- * createTextNode

foreign import javascript unsafe "$1[\"createTextNode\"]($2)"
        js_createTextNode :: JSDocument -> JSString -> IO JSTextNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.createTextNode Mozilla Document.createTextNode documentation>
createJSTextNode ::
               (MonadIO m) =>
                 JSDocument -> Text -> m (Maybe JSTextNode)
createJSTextNode document data'
  = liftIO
      ((js_createTextNode document
          (textToJSString data'))
         >>= return . Just)

-- * Events

newtype EventTarget = EventTarget { unEventTarget :: JSVal }

instance Eq (EventTarget) where
  (EventTarget a) == (EventTarget b) = js_eq a b

instance ToJSVal EventTarget where
  toJSVal = return . unEventTarget
  {-# INLINE toJSVal #-}

instance FromJSVal EventTarget where
  fromJSVal = return . fmap EventTarget . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

class IsEventTarget o where
    toEventTarget :: o -> EventTarget
--    toEventTarget = EventTarget

instance IsEventTarget JSElement where
    toEventTarget = EventTarget . unJSElement

-- FIXME: Element is overly restrictive
foreign import javascript unsafe "$1[\"addEventListener\"]($2, $3,\n$4)"
   js_addEventListener ::
       EventTarget -> JSString -> Callback (IO ()) -> Bool -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.addEventListener Mozilla EventTarget.addEventListener documentation>
addEventListener ::
                 (MonadIO m, IsEventTarget self) =>
                   self -> EventType -> Callback (IO ()) -> Bool -> m ()
addEventListener self type' listener useCapture
  = liftIO
      (js_addEventListener (toEventTarget self)
         type''
         listener
--         (maybe jsNull pToJSVal listener)
         useCapture)
             where
               type'' = case type' of
                          Change -> JS.pack "change"
                          Click  -> JS.pack "click"
                          Input  -> JS.pack "input"
                          Blur   -> JS.pack "blur"
                          ReadyStateChange -> JS.pack "readystatechange"
                          EventTxt s -> s


-- * XMLHttpRequest
newtype XMLHttpRequest = XMLHttpRequest { unXMLHttpRequest :: JSVal }

instance Eq (XMLHttpRequest) where
  (XMLHttpRequest a) == (XMLHttpRequest b) = js_eq a b

instance IsEventTarget XMLHttpRequest where
    toEventTarget = EventTarget . unXMLHttpRequest

{-
instance PToJSVal XMLHttpRequest where
  pToJSVal = unXMLHttpRequest
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLHttpRequest where
  pFromJSVal = XMLHttpRequest
  {-# INLINE pFromJSVal #-}
-}
instance ToJSVal XMLHttpRequest where
  toJSVal = return . unXMLHttpRequest
  {-# INLINE toJSVal #-}

instance FromJSVal XMLHttpRequest where
  fromJSVal = return . fmap XMLHttpRequest . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "new window[\"XMLHttpRequest\"]()"
        js_newXMLHttpRequest :: IO XMLHttpRequest

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest Mozilla XMLHttpRequest documentation>
newXMLHttpRequest :: (MonadIO m) => m XMLHttpRequest
newXMLHttpRequest
  = liftIO js_newXMLHttpRequest

foreign import javascript unsafe "$1[\"open\"]($2, $3, $4)"
        js_open ::
        XMLHttpRequest ->
          JSString -> JSString -> Bool -> {- JSString -> JSString -> -} IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.open Mozilla XMLHttpRequest.open documentation>
open ::
     (MonadIO m) =>
       XMLHttpRequest -> Text -> Text -> Bool -> m ()
open self method url async
  = liftIO (js_open self (textToJSString method) (textToJSString url) async)

foreign import javascript unsafe "$1[\"setRequestHeader\"]($2,$3)"
        js_setRequestHeader
            :: XMLHttpRequest
            -> JSString
            -> JSString
            -> IO ()

setRequestHeader :: (MonadIO m) =>
                    XMLHttpRequest
                 -> Text
                 -> Text
                 -> m ()
setRequestHeader self header value =
    liftIO (js_setRequestHeader self (textToJSString header) (textToJSString value))

-- foreign import javascript interruptible "h$dom$sendXHR($1, $2, $c);" js_send :: JSVal XMLHttpRequest -> JSVal () -> IO Int
{-
-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
send :: (MonadIO m) => XMLHttpRequest -> m ()
send self = liftIO $ js_send (unXMLHttpRequest self) jsNull >> return () -- >>= throwXHRError
-}

foreign import javascript unsafe "$1[\"send\"]()" js_send ::
        XMLHttpRequest -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
send :: (MonadIO m) => XMLHttpRequest -> m ()
send self =
    liftIO $ js_send self >> return () -- >>= throwXHRError

foreign import javascript unsafe "$1[\"send\"]($2)" js_sendString ::
        XMLHttpRequest -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
sendString :: (MonadIO m) => XMLHttpRequest -> JSString -> m ()
sendString self str =
    liftIO $ js_sendString self str >> return () -- >>= throwXHRError

foreign import javascript unsafe "$1[\"send\"]($2)" js_sendArrayBuffer ::
        XMLHttpRequest -> JSVal -> IO ()

sendArrayBuffer :: (MonadIO m) => XMLHttpRequest -> Buffer -> m ()
sendArrayBuffer xhr buf =
    liftIO $ do ref <- fmap (pToJSVal . getArrayBuffer) (thaw buf)
                js_sendArrayBuffer xhr ref

foreign import javascript unsafe "$1[\"send\"]($2)" js_sendData ::
        XMLHttpRequest
    -> JSVal
    -> IO ()

foreign import javascript unsafe "$1[\"readyState\"]"
        js_getReadyState :: XMLHttpRequest -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.readyState Mozilla XMLHttpRequest.readyState documentation>
getReadyState :: (MonadIO m) => XMLHttpRequest -> m Word
getReadyState self
  = liftIO (js_getReadyState self)

foreign import javascript unsafe "$1[\"responseType\"]"
        js_getResponseType ::
        XMLHttpRequest -> IO JSString -- XMLHttpRequestResponseType

-- | <Https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseType Mozilla XMLHttpRequest.responseType documentation>
getResponseType ::
                (MonadIO m) => XMLHttpRequest -> m Text
getResponseType self
  = liftIO (textFromJSString <$> js_getResponseType self)


foreign import javascript unsafe "$1[\"responseType\"] = $2"
        js_setResponseType ::
        XMLHttpRequest -> JSString -> IO () -- XMLHttpRequestResponseType

setResponseType :: (MonadIO m) =>
                   XMLHttpRequest
                -> Text
                -> m ()
setResponseType self typ =
    liftIO $ js_setResponseType self (textToJSString typ)

data XMLHttpRequestResponseType = XMLHttpRequestResponseType
                                | XMLHttpRequestResponseTypeArraybuffer
                                | XMLHttpRequestResponseTypeBlob
                                | XMLHttpRequestResponseTypeDocument
                                | XMLHttpRequestResponseTypeJson
                                | XMLHttpRequestResponseTypeText
foreign import javascript unsafe "\"\""
        js_XMLHttpRequestResponseType :: JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"arraybuffer\""
        js_XMLHttpRequestResponseTypeArraybuffer ::
        JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"blob\""
        js_XMLHttpRequestResponseTypeBlob ::
        JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"document\""
        js_XMLHttpRequestResponseTypeDocument ::
        JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"json\""
        js_XMLHttpRequestResponseTypeJson ::
        JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"text\""
        js_XMLHttpRequestResponseTypeText ::
        JSVal -- XMLHttpRequestResponseType

{-
instance PToJSVal XMLHttpRequestResponseType where
        pToJSVal XMLHttpRequestResponseType = js_XMLHttpRequestResponseType
        pToJSVal XMLHttpRequestResponseTypeArraybuffer
          = js_XMLHttpRequestResponseTypeArraybuffer
        pToJSVal XMLHttpRequestResponseTypeBlob
          = js_XMLHttpRequestResponseTypeBlob
        pToJSVal XMLHttpRequestResponseTypeDocument
          = js_XMLHttpRequestResponseTypeDocument
        pToJSVal XMLHttpRequestResponseTypeJson
          = js_XMLHttpRequestResponseTypeJson
        pToJSVal XMLHttpRequestResponseTypeText
          = js_XMLHttpRequestResponseTypeText
-}
instance ToJSVal XMLHttpRequestResponseType where
        toJSVal XMLHttpRequestResponseType
          = return js_XMLHttpRequestResponseType
        toJSVal XMLHttpRequestResponseTypeArraybuffer
          = return js_XMLHttpRequestResponseTypeArraybuffer
        toJSVal XMLHttpRequestResponseTypeBlob
          = return js_XMLHttpRequestResponseTypeBlob
        toJSVal XMLHttpRequestResponseTypeDocument
          = return js_XMLHttpRequestResponseTypeDocument
        toJSVal XMLHttpRequestResponseTypeJson
          = return js_XMLHttpRequestResponseTypeJson
        toJSVal XMLHttpRequestResponseTypeText
          = return js_XMLHttpRequestResponseTypeText

{-
instance PFromJSVal XMLHttpRequestResponseType where
        pFromJSVal x
          | x == js_XMLHttpRequestResponseType = XMLHttpRequestResponseType
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeArraybuffer =
            XMLHttpRequestResponseTypeArraybuffer
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeBlob =
            XMLHttpRequestResponseTypeBlob
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeDocument =
            XMLHttpRequestResponseTypeDocument
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeJson =
            XMLHttpRequestResponseTypeJson
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeText =
            XMLHttpRequestResponseTypeText
-}
instance FromJSVal XMLHttpRequestResponseType where
--        fromJSValUnchecked = return . pFromJSVal
        fromJSVal x
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

foreign import javascript unsafe "$1[\"response\"]" js_getResponse
        :: XMLHttpRequest
        -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.response Mozilla XMLHttpRequest.response documentation>
getResponse :: (MonadIO m) =>
               XMLHttpRequest
            -> m JSVal
getResponse self =
    liftIO (js_getResponse self)

foreign import javascript unsafe "$1[\"responseText\"]"
        js_getResponseText :: XMLHttpRequest -> IO JSString

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseText Mozilla XMLHttpRequest.responseText documentation>
getResponseText ::
                (MonadIO m) => XMLHttpRequest -> m Text
getResponseText self
  = liftIO
      (textFromJSString <$> js_getResponseText self)

foreign import javascript unsafe "$1[\"status\"]" js_getStatus ::
        XMLHttpRequest -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.status Mozilla XMLHttpRequest.status documentation>
getStatus :: (MonadIO m) => XMLHttpRequest -> m Word
getStatus self = liftIO (js_getStatus self)

foreign import javascript unsafe "$1[\"statusText\"]"
        js_getStatusText :: XMLHttpRequest -> IO JSString

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.statusText Mozilla XMLHttpRequest.statusText documentation>
getStatusText ::
              (MonadIO m) => XMLHttpRequest -> m Text
getStatusText self
  = liftIO
      (textFromJSString <$> js_getStatusText self)

foreign import javascript unsafe "$1[\"responseURL\"]"
        js_getResponseURL :: XMLHttpRequest -> IO JSString


-- * Pure HTML

data EventType
    = Change
    | Click
    | Input
    | Blur
    | ReadyStateChange
    | EventTxt JSString

data Attr action
    = Attr Text Text
    | Event (EventType, Maybe JSString -> action)


-- | FIXME: this instances is not really right, but was added for the sake of the test suite
instance Eq (Attr action) where
  (Attr k1 v1) == (Attr k2 v2) = (k1 == k2) && (v1 == v2)
  _ == _ = False
{-
instance ToJSString EventType where
    toJSString Change = toJSString "change"
    toJSString Click  = toJSString "click"
    toJSString Input  = toJSString "input"
    toJSString Blur   = toJSString "blur"
-}
data HTML action
  = forall a. Element { elementName        :: Text
                      , elementAttrs       :: [Attr action]
                      , elementKey         :: Maybe Text
                      , elementDescendants :: Int
                      , elementChildren    :: [HTML action]
                      }
  | CDATA Bool Text
--   | Children [HTML action]
    deriving Eq

instance Show (Attr action) where
    show (Attr k v) = (Text.unpack k) <> " := " <> (Text.unpack v) <> " "
    show (Event eventType) = "Event "

instance Show (HTML action) where
    show (Element tagName attrs _key _count children) =
        (Text.unpack tagName) <> " [" <> concat (map show attrs) <> "]\n" <> concat (map showChild children)
        where
          showChild c = "    " <> show c <> "\n"
    show (CDATA b txt) = Text.unpack txt

descendants :: [HTML action] -> Int
descendants elems = sum [ d | Element _ _ _ d _ <- elems] + (length elems)

{-
flattenHTML :: HTML action -> HTML action
flattenHTML h@(CDATA _ _) = h
flattenHTML h@(Children _) = h
flattenHTML (Element t acts attrs children)
-}
renderHTML :: forall action m. (MonadIO m) => (action -> IO ()) -> JSDocument -> HTML action -> m (Maybe JSNode)
renderHTML _ doc (CDATA _ t) = fmap (fmap toJSNode) $ createJSTextNode doc t
renderHTML handle doc (Element tag {- events -} attrs _ _ children) =
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
          do cb <- asyncCallback (handle' elem toAction) -- FIXME: free ?
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

-- * Canvas

newtype JSContext2D = JSContext2D { unJSContext :: JSVal }

instance ToJSVal JSContext2D where
  toJSVal = return . unJSContext
  {-# INLINE toJSVal #-}

instance FromJSVal JSContext2D where
  fromJSVal = return . fmap JSContext2D . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"getContext\"](\"2d\")"
        js_getContext2d ::
        JSElement -> IO JSVal

getContext2D :: (MonadIO m) => JSElement -> m (Maybe JSContext2D)
getContext2D elem = liftIO $ fromJSVal =<< js_getContext2d elem

foreign import javascript unsafe "$1[\"fillRect\"]($2, $3, $4, $5)"
        js_fillRect ::
        JSContext2D -> Double -> Double -> Double -> Double -> IO ()

fillRect :: JSContext2D -> Double -> Double -> Double -> Double -> IO ()
fillRect = js_fillRect

renderColor :: Color -> JSString
renderColor (ColorName c) = c

renderStyle :: Style -> JSString
renderStyle (StyleColor color) = renderColor color

foreign import javascript unsafe "$1[\"fillStyle\"] = $2"
        js_fillStyle ::
        JSContext2D -> JSString -> IO ()

setFillStyle :: JSContext2D -> Style -> IO ()
setFillStyle ctx style = js_fillStyle ctx (renderStyle style)

foreign import javascript unsafe "$1[\"strokeStyle\"] = $2"
        js_strokeStyle ::
        JSContext2D -> JSString -> IO ()

setStrokeStyle :: JSContext2D -> Style -> IO ()
setStrokeStyle ctx style = js_strokeStyle ctx (renderStyle style)

foreign import javascript unsafe "$1[\"save\"]()"
        js_save ::
        JSContext2D -> IO ()

save :: (MonadIO m) => JSContext2D -> m ()
save = liftIO . js_save

foreign import javascript unsafe "$1[\"restore\"]()"
        js_restore ::
        JSContext2D -> IO ()

restore :: (MonadIO m) => JSContext2D -> m ()
restore = liftIO . js_restore

foreign import javascript unsafe "$1[\"moveTo\"]($2, $3)"
        js_moveTo ::
        JSContext2D -> Double -> Double -> IO ()

moveTo :: (MonadIO m) => JSContext2D -> Double -> Double -> m ()
moveTo ctx x y = liftIO $ js_moveTo ctx x y

foreign import javascript unsafe "$1[\"lineTo\"]($2, $3)"
        js_lineTo ::
        JSContext2D -> Double -> Double -> IO ()

lineTo :: (MonadIO m) => JSContext2D -> Double -> Double -> m ()
lineTo ctx x y = liftIO $ js_lineTo ctx x y


foreign import javascript unsafe "$1[\"arc\"]($2, $3, $4, $5, $6, $7)"
        js_arc ::
        JSContext2D -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()

arc :: (MonadIO m) => JSContext2D -> Double -> Double -> Double -> Double -> Double -> Bool -> m ()
arc ctx x y radius startAngle endAngle counterClockwise = liftIO $ js_arc ctx x y radius startAngle endAngle counterClockwise

foreign import javascript unsafe "$1[\"beginPath\"]()"
        js_beginPath ::
        JSContext2D -> IO ()

beginPath :: (MonadIO m) => JSContext2D -> m ()
beginPath = liftIO . js_beginPath

foreign import javascript unsafe "$1[\"stroke\"]()"
        js_stroke ::
        JSContext2D -> IO ()

stroke :: (MonadIO m) => JSContext2D -> m ()
stroke = liftIO . js_stroke

foreign import javascript unsafe "$1[\"fill\"]()"
        js_fill ::
        JSContext2D -> IO ()

fill :: (MonadIO m) => JSContext2D -> m ()
fill = liftIO . js_fill

-- * Font/Text

foreign import javascript unsafe "$1[\"font\"] = $2"
        js_font ::
        JSContext2D -> JSString -> IO ()

setFont :: (MonadIO m) => JSContext2D -> JSString -> m ()
setFont ctx font = liftIO $ js_font ctx font

foreign import javascript unsafe "$1[\"textAlign\"] = $2"
        js_textAlign ::
        JSContext2D -> JSString -> IO ()

setTextAlign :: (MonadIO m) => JSContext2D -> JSString -> m ()
setTextAlign ctx align = liftIO $ js_textAlign ctx align

foreign import javascript unsafe "$1[\"fillText\"]($2, $3, $4)"
  js_fillText :: JSContext2D -> JSString -> Double -> Double -> IO ()

foreign import javascript unsafe "$1[\"fillText\"]($2, $3, $4, $5)"
        js_fillTextMaxWidth ::
        JSContext2D -> JSString -> Double -> Double -> Double -> IO ()

fillText :: (MonadIO m) =>
            JSContext2D
         -> JSString
         -> Double
         -> Double
         -> Maybe Double
         -> m ()
fillText ctx txt x y Nothing = liftIO $ js_fillText ctx txt x y
fillText ctx txt x y (Just maxWidth) = liftIO $ js_fillTextMaxWidth ctx txt x y maxWidth

foreign import javascript unsafe "$1[\"scale\"]($2, $3)"
  js_scale :: JSContext2D -> Int -> Int -> IO ()

scale :: (MonadIO m) => JSContext2D -> Int -> Int -> m ()
scale ctx x y = liftIO $ js_scale ctx x y

data Gradient = Gradient
data Pattern = Pattern

type Percentage = Double
type Alpha = Double

data Color
  = ColorName JSString
  | RGBA Percentage Percentage Percentage Alpha

data Style
  = StyleColor Color
  | StyleGradient Gradient
  | StylePattern Pattern

data Rect
  = Rect { _rectX      :: Double
         , _rectY      :: Double
         , _rectWidth  :: Double
         , _rectHeight :: Double
         }

-- https://developer.mozilla.org/en-US/docs/Web/API/Path2D
data Path2D
  = MoveTo Double Double
  | LineTo Double Double
  | PathRect Rect
  | Arc Double Double Double Double Double Bool

data Draw
  = FillRect Rect
  | Stroke [Path2D]
  | Fill [Path2D]
  | FillText JSString Double Double (Maybe Double)

-- | this is not sustainable. A Set of attributes is probably a better choice
data Context2D = Context2D
 { _fillStyle   :: Style
 , _strokeStyle :: Style
 , _lineWidth   :: Double
 , _font        :: JSString
 , _textAlign   :: JSString
 }

makeLenses ''Context2D

context2D :: Context2D
context2D = Context2D
  { _fillStyle   = StyleColor (ColorName $ JS.pack "black") -- technically should be #000
  , _strokeStyle = StyleColor (ColorName $ JS.pack "black") -- technically should be #000
  , _lineWidth   = 1.0
  , _font        = JS.pack "10px sans-serif"
  , _textAlign   = JS.pack "left"
  }

data Canvas = Canvas
  { _canvasId :: Text
  , _canvas :: Canvas2D
  }

data Canvas2D
  = WithContext2D Context2D [ Canvas2D ]
  | Draw Draw


mkPath :: (MonadIO m) => JSContext2D -> [Path2D] -> m ()
mkPath ctx segments =
  do beginPath ctx
     mapM_ (mkSegment ctx) segments
  where
    mkSegment ctx segment =
      case segment of
       (MoveTo x y) -> moveTo ctx x y
       (LineTo x y) -> lineTo ctx x y
       (Arc x y radius startAngle endAngle counterClockwise) -> arc ctx x y radius startAngle endAngle counterClockwise
--        (Rect (Rect x y w h)) -> rect x y w h


-- https://gist.github.com/joubertnel/870190
drawCanvas :: Canvas -> IO ()
drawCanvas (Canvas cid content) =
  do (Just document) <- currentDocument
     mCanvasElem <- getElementById document (textToJSString cid)
     case mCanvasElem of
      Nothing       -> pure ()
      (Just canvasElem) ->
        do js_setAttribute canvasElem (JS.pack "width")  (JS.pack "1920")
           js_setAttribute canvasElem (JS.pack "height") (JS.pack "960")
           setStyle canvasElem (JS.pack "width")  (JS.pack "960px")
           setStyle canvasElem (JS.pack "height")  (JS.pack "480px")
           mctx <- getContext2D canvasElem
           case mctx of
            Nothing -> pure ()
            (Just ctx) -> do
              scale ctx 2 2
              drawCanvas' ctx context2D content
  where
    drawCanvas' ctx ctx2d (Draw (FillRect (Rect x y w h))) =
      fillRect ctx x y w h
    drawCanvas' ctx ctx2d (Draw (Stroke path2D)) =
      do mkPath ctx path2D
         stroke ctx
    drawCanvas' ctx ctx2d (Draw (Fill path2D)) =
      do mkPath ctx path2D
         fill ctx
    drawCanvas' ctx ctx2d (Draw (FillText text x y maxWidth)) =
      do fillText ctx text x y maxWidth
    drawCanvas' ctx oldCtx2d (WithContext2D ctx2d content) =
     do save ctx
        setFillStyle ctx (ctx2d ^. fillStyle)
        setStrokeStyle ctx (ctx2d ^. strokeStyle)
        setFont ctx (ctx2d ^. font)
        setTextAlign ctx (ctx2d ^. textAlign)
        mapM_ (drawCanvas' ctx ctx2d) content -- NOTE: how to do we deal with things that children have messed with in the ctx? save()/restore()?
        restore ctx

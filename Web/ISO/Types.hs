{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances, GADTs, JavaScriptFFI, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
module Web.ISO.Types where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (fromJust)
import Data.Aeson.Types (Parser, Result(..), parse)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import qualified Data.Text as Text
-- import GHCJS.Prim (ToJSString(..), FromJSString(..))
-- import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import GHCJS.Buffer
import GHCJS.Foreign (jsNull)
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback1, syncCallback1)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Nullable (Nullable(..), nullableToMaybe, maybeToNullable)
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

instance IsEventTarget JSDocument where
    toEventTarget = EventTarget . unJSDocument

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document Mozilla Document documentation>
newJSDocument :: (MonadIO m) => m JSDocument
newJSDocument = liftIO js_newDocument

foreign import javascript unsafe "$r = document"
  ghcjs_currentDocument :: IO JSDocument

currentDocument :: IO (Maybe JSDocument)
currentDocument = Just <$> ghcjs_currentDocument

-- * JSWindow

newtype JSWindow = JSWindow { unJSWindow ::  JSVal }

instance ToJSVal JSWindow where
  toJSVal = return . unJSWindow
  {-# INLINE toJSVal #-}

instance FromJSVal JSWindow where
  fromJSVal = return . fmap JSWindow . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$r = window"
  js_window :: IO JSWindow

instance IsEventTarget JSWindow where
    toEventTarget = EventTarget . unJSWindow

window :: (MonadIO m) => m JSWindow
window = liftIO js_window

foreign import javascript unsafe "$1[\"devicePixelRatio\"]"
  js_devicePixelRatio :: JSWindow -> IO JSVal

devicePixelRatio :: (MonadIO m) => JSWindow -> m (Maybe Double)
devicePixelRatio w = liftIO (fromJSVal =<< js_devicePixelRatio w)

foreign import javascript unsafe "$1[\"getSelection\"]()"
  js_getSelection :: JSWindow -> IO Selection

getSelection :: (MonadIO m) => JSWindow -> m Selection
getSelection w = liftIO (js_getSelection w)

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

foreign import javascript unsafe "$1[\"clientLeft\"]"
        js_getClientLeft :: JSElement -> IO Double

getClientLeft :: (MonadIO m) => JSElement -> m Double
getClientLeft = liftIO . js_getClientLeft

foreign import javascript unsafe "$1[\"clientTop\"]"
        js_getClientTop :: JSElement -> IO Double

getClientTop :: (MonadIO m) => JSElement -> m Double
getClientTop = liftIO . js_getClientTop

foreign import javascript unsafe "$1[\"clientWidth\"]"
        js_getClientWidth :: JSElement -> IO Double

getClientWidth :: (MonadIO m) => JSElement -> m Double
getClientWidth = liftIO . js_getClientWidth

foreign import javascript unsafe "$1[\"clientHeight\"]"
        js_getClientHeight :: JSElement -> IO Double

getClientHeight :: (MonadIO m) => JSElement -> m Double
getClientHeight = liftIO . js_getClientHeight

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

-- * innerHTML

foreign import javascript unsafe "$1[\"innerHTML\"] = $2"
        js_setInnerHTML :: JSElement -> JSString -> IO ()

setInnerHTML :: (MonadIO m) => JSElement -> JSString -> m ()
setInnerHTML elm content = liftIO $ js_setInnerHTML elm content

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


foreign import javascript unsafe "$1[\"getAttribute\"]($2)"
        js_getAttribute :: JSElement -> JSString -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
getAttribute :: (MonadIO m) =>
                JSElement
             -> JSString
             -> m (Maybe JSString)
getAttribute self name = liftIO (js_getAttribute self name >>= fromJSVal)

foreign import javascript unsafe "$1[\"style\"][$2] = $3"
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

-- * dataset

foreign import javascript unsafe "$1[\"dataset\"][$2]"
        js_getData :: JSNode -> JSString -> IO (Nullable JSString)

getData :: (MonadIO m, IsJSNode self) => self -> JSString -> m (Maybe JSString)
getData self name = liftIO (nullableToMaybe <$> js_getData (toJSNode self) name)
--getData self name = liftIO (fmap fromJSVal <$> maybeJSNullOrUndefined <$> (js_getData (toJSNode self) name))

foreign import javascript unsafe "$1[\"dataset\"][$2] = $3"
        js_setData :: JSNode -> JSString -> JSString -> IO ()

setData :: (MonadIO m, IsJSNode self) => self -> JSString -> JSString -> m ()
setData self name value = liftIO (js_setData (toJSNode self) name value)

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


class IsEvent ev where
  eventToJSString :: ev -> JSString

data Event
  = ReadyStateChange
  deriving (Eq, Show, Read)

instance IsEvent Event where
  eventToJSString ReadyStateChange = JS.pack "readystatechange"

data MouseEvent
  = Click
  | ContextMenu
  | DblClick
  | MouseDown
  | MouseEnter
  | MouseLeave
  | MouseMove
  | MouseOver
  | MouseOut
  | MouseUp
    deriving (Eq, Show, Read)

instance IsEvent MouseEvent where
  eventToJSString Click       = JS.pack "click"
  eventToJSString ContextMenu = JS.pack "contextmenu"
  eventToJSString DblClick    = JS.pack "dblclick"
  eventToJSString MouseDown   = JS.pack "MouseDown"
  eventToJSString MouseEnter  = JS.pack "MouseEnter"
  eventToJSString MouseLeave  = JS.pack "MouseLeave"
  eventToJSString MouseMove   = JS.pack "MouseMove"
  eventToJSString MouseOver   = JS.pack "MouseOver"
  eventToJSString MouseOut    = JS.pack "MouseOut"
  eventToJSString MouseUp     = JS.pack "MouseUp"

data KeyboardEvent
  = KeyDown
  | KeyPress
  | KeyUp
    deriving (Eq, Show, Read)

instance IsEvent KeyboardEvent where
  eventToJSString KeyDown  = JS.pack "keydown"
  eventToJSString KeyPress = JS.pack "keypress"
  eventToJSString KeyUp    = JS.pack "keyup"

data FrameEvent
  = FrameAbort
  | BeforeUnload
  | FrameError
  | HashChange
  | FrameLoad
  | PageShow
  | PageHide
  | Resize
  | Scroll
  | Unload
    deriving (Eq, Show, Read)

data FormEvent
  = Blur
  | Change
  | Focus
  | FocusIn
  | FocusOut
  | Input
  | Invalid
  | Reset
  | Search
  | Select
  | Submit
  deriving (Eq, Show, Read)

instance IsEvent FormEvent where
  eventToJSString Blur     = JS.pack "blur"
  eventToJSString Change   = JS.pack "change"
  eventToJSString Focus    = JS.pack "focus"
  eventToJSString FocusIn  = JS.pack "focusin"
  eventToJSString FocusOut = JS.pack "focusout"
  eventToJSString Input    = JS.pack "input"
  eventToJSString Invalid  = JS.pack "invalid"
  eventToJSString Reset    = JS.pack "reset"
  eventToJSString Search   = JS.pack "search"
  eventToJSString Select   = JS.pack "select"
  eventToJSString Submit   = JS.pack "submit"

data DragEvent
  = Drag
  | DragEnd
  | DragEnter
  | DragLeave
  | DragOver
  | DragStart
  | Drop
  deriving (Eq, Show, Read)

data PrintEvent
  = AfterPrint
  | BeforePrint
  deriving (Eq, Show, Read)

data MediaEvent
  = CanPlay
  | CanPlayThrough
  | DurationChange
  | Emptied
  | Ended
  | MediaError
  | LoadedData
  | LoadedMetaData
  | Pause
  | Play
  | Playing
  | RateChange
  | Seeked
  | Seeking
  | Stalled
  | Suspend
  | TimeUpdate
  | VolumeChange
  | Waiting
  deriving (Eq, Show, Read)

data ProgressEvent
  = LoadStart
  | Progress
  | ProgressAbort
  | ProgressError
  | ProgressLoad
  | Timeout
  | LoadEnd
  deriving (Eq, Show, Read)

instance IsEvent ProgressEvent where
  eventToJSString LoadStart     = JS.pack "loadstart"
  eventToJSString Progress      = JS.pack "progress"
  eventToJSString ProgressAbort = JS.pack "abort"
  eventToJSString ProgressError = JS.pack "error"
  eventToJSString ProgressLoad  = JS.pack "load"
  eventToJSString Timeout       = JS.pack "timeout"
  eventToJSString LoadEnd       = JS.pack "loadend"

data AnimationEvent
  = AnimationEnd
  | AnimationInteration
  | AnimationStart
  deriving (Eq, Show, Read)

data TransitionEvent
  = TransitionEnd
  deriving (Eq, Show, Read)

data ServerSentEvent
  = ServerError
  | ServerMessage
  | Open
    deriving (Eq, Show, Read)

data MiscEvent
  = MiscMessage
  | Online
  | Offline
  | PopState
  | MiscShow
  | Storage
  | Toggle
  | Wheel
  deriving (Eq, Show, Read)

data TouchEvent
  = TouchCancel
  | TouchEnd
  | TouchMove
  | TouchStart
  deriving (Eq, Show, Read)

-- https://developer.mozilla.org/en-US/docs/Web/API/ProgressEvent
-- data ProgressEvent =

data EventType
  = MouseEvent MouseEvent
  | KeyboardEvent KeyboardEvent
  | FrameEvent FrameEvent
  | FormEvent FormEvent
  | DragEvent DragEvent
  | ClipboardEvent ClipboardEvent
  | PrintEvent PrintEvent
  | MediaEvent MediaEvent
  | AnimationEvent AnimationEvent
  | TransitionEvent TransitionEvent
  | ServerSentEvent ServerSentEvent
  | MiscEvent MiscEvent
  | TouchEvent TouchEvent
  deriving (Eq, Show, Read)

-- * Event Objects

-- http://www.w3schools.com/jsref/dom_obj_event.asp

class IsEventObject obj where
  asEventObject        :: obj -> EventObject

-- * EventObject

newtype EventObject = EventObject { unEventObject :: JSVal }

instance ToJSVal EventObject where
  toJSVal = return . unEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal EventObject where
  fromJSVal = return . fmap EventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject EventObject where
  asEventObject = id

foreign import javascript unsafe "$1[\"defaultPrevented\"]" js_defaultPrevented ::
        EventObject -> IO Bool

defaultPrevented :: (IsEventObject obj, MonadIO m) => obj -> m Bool
defaultPrevented obj = liftIO (js_defaultPrevented (asEventObject obj))

foreign import javascript unsafe "$1[\"target\"]" js_target ::
        EventObject -> IO JSVal

target :: (IsEventObject obj, MonadIO m) => obj -> m JSElement
target obj = liftIO (fromJSValUnchecked =<< (js_target (asEventObject obj)))

foreign import javascript unsafe "$1[\"preventDefault\"]()" js_preventDefault ::
        EventObject -> IO ()

preventDefault :: (IsEventObject obj, MonadIO m) => obj -> m ()
preventDefault obj = liftIO (js_preventDefault (asEventObject obj))

foreign import javascript unsafe "$1[\"stopPropagation\"]()" js_stopPropagation ::
        EventObject -> IO ()

stopPropagation :: (IsEventObject obj, MonadIO m) => obj -> m ()
stopPropagation obj = liftIO (js_stopPropagation (asEventObject obj))

-- * MouseEventObject

newtype MouseEventObject = MouseEventObject { unMouseEventObject :: JSVal }

instance Show MouseEventObject where
  show _ = "MouseEventObject"

instance ToJSVal MouseEventObject where
  toJSVal = return . unMouseEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal MouseEventObject where
  fromJSVal = return . fmap MouseEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject MouseEventObject where
  asEventObject (MouseEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"clientX\"]" clientX ::
        MouseEventObject -> Double

foreign import javascript unsafe "$1[\"clientY\"]" clientY ::
        MouseEventObject -> Double

foreign import javascript unsafe "$1[\"button\"]" button ::
        MouseEventObject -> Int

-- * KeyboardEventObject

newtype KeyboardEventObject = KeyboardEventObject { unKeyboardEventObject :: JSVal }

instance ToJSVal KeyboardEventObject where
  toJSVal = return . unKeyboardEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal KeyboardEventObject where
  fromJSVal = return . fmap KeyboardEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject KeyboardEventObject where
  asEventObject (KeyboardEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"charCode\"]" charCode ::
        KeyboardEventObject -> Int

foreign import javascript unsafe "$1[\"keyCode\"]" keyCode ::
        KeyboardEventObject -> Int

-- * ProgressEventObject

newtype ProgressEventObject = ProgressEventObject { unProgressEventObject :: JSVal }

instance ToJSVal ProgressEventObject where
  toJSVal = return . unProgressEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal ProgressEventObject where
  fromJSVal = return . fmap ProgressEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

-- charCode :: (MonadIO m) => KeyboardEventObject -> IO 

{-
-- * FormEventObject

newtype FormEventObject = FormEventObject { unFormEventObject :: JSVal }

instance ToJSVal FormEventObject where
  toJSVal = return . unFormEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal FormEventObject where
  fromJSVal = return . fmap FormEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}
-}
-- * EventObjectOf

type family EventObjectOf event :: *
type instance EventObjectOf Event          = EventObject
type instance EventObjectOf MouseEvent     = MouseEventObject
type instance EventObjectOf KeyboardEvent  = KeyboardEventObject
type instance EventObjectOf FormEvent      = EventObject
type instance EventObjectOf ProgressEvent  = ProgressEventObject
type instance EventObjectOf ClipboardEvent = ClipboardEventObject

-- * DOMRect

newtype DOMClientRect = DomClientRect { unDomClientRect :: JSVal }

foreign import javascript unsafe "$1[\"width\"]" width ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"top\"]" rectTop ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"left\"]" rectLeft ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"right\"]" rectRight ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"bottom\"]" rectBottom ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"height\"]" height ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"getBoundingClientRect\"]()" js_getBoundingClientRect ::
  JSElement -> IO DOMClientRect

getBoundingClientRect :: (MonadIO m) => JSElement -> m DOMClientRect
getBoundingClientRect = liftIO . js_getBoundingClientRect

-- * addEventListener

-- FIXME: Element is overly restrictive
foreign import javascript unsafe "$1[\"addEventListener\"]($2, $3,\n$4)"
   js_addEventListener :: EventTarget -> JSString -> Callback (JSVal -> IO ()) -> Bool -> IO ()

addEventListener :: (MonadIO m, IsEventTarget self, IsEvent event, FromJSVal (EventObjectOf event)) =>
                  self
               -> event
               -> (EventObjectOf event -> IO ())
               -> Bool
               -> m ()
addEventListener self event callback useCapture = liftIO $
  do cb <- syncCallback1 ThrowWouldBlock callback'
     js_addEventListener (toEventTarget self) (eventToJSString event) cb useCapture
  where
    callback' = \ev ->
         do (Just eventObject) <- fromJSVal ev
            callback eventObject

{-
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.addEventListener Mozilla EventTarget.addEventListener documentation>
addEventListener
  :: (MonadIO m, IsEventTarget self) =>
     self
  -> EventType
  -> Callback (IO ())
  -> Bool
  -> m ()
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
                          Keydown -> JS.pack "keydown"
                          Keyup   -> JS.pack "keyup"
                          Keypress -> JS.pack "keypress"
                          ReadyStateChange -> JS.pack "readystatechange"
                          EventTxt s -> s

-}
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

-- * Selection

newtype Selection = Selection { unSelection ::  JSVal }

instance ToJSVal Selection where
  toJSVal = pure . unSelection
  {-# INLINE toJSVal #-}

instance FromJSVal Selection where
  fromJSVal = pure . fmap Selection . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"getRangeAt\"]($2)"
        js_getRangeAt :: Selection -> Int -> IO Range

getRangeAt :: (MonadIO m) => Selection -> Int -> m Range
getRangeAt selection index = liftIO (js_getRangeAt selection index)

foreign import javascript unsafe "$1[\"rangeCount\"]"
        js_getRangeCount :: Selection -> IO Int

getRangeCount :: (MonadIO m) => Selection -> m Int
getRangeCount selection = liftIO (js_getRangeCount selection)

foreign import javascript unsafe "$1[\"toString\"]()"
 selectionToString :: Selection -> IO JSString

-- * Range

newtype Range = Range { unRange ::  JSVal }

instance ToJSVal Range where
  toJSVal = pure . unRange
  {-# INLINE toJSVal #-}

instance FromJSVal Range where
  fromJSVal = pure . fmap Range . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"startContainer\"]"
        js_getStartContainer :: Range -> IO JSNode

getStartContainer :: (MonadIO m) => Range -> m JSNode
getStartContainer r = liftIO (js_getStartContainer r)

foreign import javascript unsafe "$1[\"startOffset\"]"
        js_getStartOffset :: Range -> IO Int

getStartOffset :: (MonadIO m) => Range -> m Int
getStartOffset r = liftIO (js_getStartOffset r)

foreign import javascript unsafe "$1[\"endContainer\"]"
        js_getEndContainer :: Range -> IO JSNode

getEndContainer :: (MonadIO m) => Range -> m JSNode
getEndContainer r = liftIO (js_getEndContainer r)

foreign import javascript unsafe "$1[\"endOffset\"]"
        js_getEndOffset :: Range -> IO Int

getEndOffset :: (MonadIO m) => Range -> m Int
getEndOffset r = liftIO (js_getEndOffset r)

-- * Pure HTML

data Attr action where
  Attr  :: Text -> Text -> Attr action
  Event :: (FromJSVal (EventObjectOf event), IsEvent event) => event -> (EventObjectOf event -> IO action) -> Attr action

-- | FIXME: this instances is not really right, but was added for the sake of the test suite
instance Eq (Attr action) where
  (Attr k1 v1) == (Attr k2 v2) = (k1 == k2) && (v1 == v2)
  _ == _ = False

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
    show (Event _eventType _) = "Event " -- ++ show eventType ++ " <function>"

instance Show (HTML action) where
    show (Element tagName attrs _key _count children) =
        (Text.unpack tagName) <> " [" <> concat (map show attrs) <> "]\n" <> concat (map showChild children)
        where
          showChild c = "    " <> show c <> "\n"
    show (CDATA b txt) = Text.unpack txt

descendants :: [HTML action] -> Int
descendants elems = sum [ d | Element _ _ _ d _ <- elems] + (length elems)

renderHTML :: forall action m. (MonadIO m) => (action -> IO ()) -> JSDocument -> HTML action -> m (Maybe JSNode)
renderHTML _ doc (CDATA _ t) = fmap (fmap toJSNode) $ createJSTextNode doc t
renderHTML handle doc (Element tag {- events -} attrs _ _ children) =
    do me <- createJSElement doc tag
       case me of
         Nothing -> return Nothing
         (Just e) ->
             do mapM_ (\c -> appendChild e =<< renderHTML handle doc c) children
                mapM_ (doAttr e) attrs
                {-
                let events' = [ ev | ev@(Event ev f) <- attrs]
                    attrs'  = [ (k,v) | Attr k v <- attrs]
                liftIO $ mapM_ (\(k, v) -> setAttribute e k v) attrs'
                liftIO $ mapM_ (handleEvent e) events'
                -}
                return (Just $ toJSNode e)
    where
      doAttr elem (Attr k v)   = setAttribute elem k v
      doAttr elem (Event eventType toAction) =
           addEventListener elem eventType (\e -> handle =<< toAction e) False
{-
      handle' :: JSElement -> (Maybe JSString -> action) -> IO ()
      handle' elem toAction =
          do ms <- getValue elem
             handle (toAction ms)
-}
--       handleEvent :: JSElement -> Attr (event, EventObjectOf event -> action) -> IO ()
{-
      handleEvent elem (Event eventType toAction) =
        addEventListener elem eventType (\e -> handle =<< toAction e) False
-}
{-
          do cb <- asyncCallback (handle' elem toAction) -- FIXME: free ?
             addEventListener elem eventType cb False
-}

-- * DataTransfer

newtype DataTransfer = DataTransfer { unDataTransfer :: JSVal }

instance Show DataTransfer where
  show _ = "DataTransfer"

instance ToJSVal DataTransfer where
  toJSVal = pure . unDataTransfer
  {-# INLINE toJSVal #-}

instance FromJSVal DataTransfer where
  fromJSVal = pure . fmap DataTransfer . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"getData\"]($2)" js_getDataTransferData ::
        DataTransfer -> JSString -> IO JSString

getDataTransferData :: (MonadIO m) =>
           DataTransfer
        -> JSString -- ^ format
        -> m JSString
getDataTransferData dt format = liftIO (js_getDataTransferData dt format)

foreign import javascript unsafe "$1[\"setData\"]($2, $3)" js_setDataTransferData ::
        DataTransfer -> JSString -> JSString -> IO ()

setDataTransferData :: (MonadIO m) =>
                       DataTransfer
                    -> JSString -- ^ format
                    -> JSString -- ^ data
                    -> m ()
setDataTransferData dataTransfer format data_ = liftIO (js_setDataTransferData dataTransfer format data_)

-- * Clipboard

data ClipboardEvent
  = Copy
  | Cut
  | Paste
    deriving (Eq, Show, Read)

instance IsEvent ClipboardEvent where
  eventToJSString Copy  = JS.pack "copy"
  eventToJSString Cut   = JS.pack "cut"
  eventToJSString Paste = JS.pack "paste"

-- * ClipboardEventObject

newtype ClipboardEventObject = ClipboardEventObject { unClipboardEventObject :: JSVal }

instance Show ClipboardEventObject where
  show _ = "ClipboardEventObject"

instance ToJSVal ClipboardEventObject where
  toJSVal = pure . unClipboardEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal ClipboardEventObject where
  fromJSVal = pure . fmap ClipboardEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject ClipboardEventObject where
  asEventObject (ClipboardEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"clipboardData\"]" clipboardData ::
        ClipboardEventObject -> IO DataTransfer

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


foreign import javascript unsafe "$1[\"clearRect\"]($2, $3, $4, $5)"
        js_clearRect ::
        JSContext2D -> Double -> Double -> Double -> Double -> IO ()

clearRect :: JSContext2D -> Double -> Double -> Double -> Double -> IO ()
clearRect = js_clearRect

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

foreign import javascript unsafe "$1[\"lineWidth\"] = $2"
        js_setLineWidth ::
        JSContext2D -> Double -> IO ()

setLineWidth :: (MonadIO m) => JSContext2D -> Double -> m ()
setLineWidth ctx w = liftIO $ js_setLineWidth ctx w


-- * Font/Text

foreign import javascript unsafe "$1[\"font\"] = $2"
        js_font ::
        JSContext2D -> JSString -> IO ()

setFont :: (MonadIO m) => JSContext2D -> JSString -> m ()
setFont ctx font = liftIO $ js_font ctx font

foreign import javascript unsafe "$1[\"textAlign\"] = $2"
        js_textAlign ::
        JSContext2D -> JSString -> IO ()

data TextAlign
  = AlignStart
  | AlignEnd
  | AlignLeft
  | AlignCenter
  | AlignRight
    deriving (Eq, Show, Read)

textAlignToJSString :: TextAlign -> JSString
textAlignToJSString AlignStart  = JS.pack "start"
textAlignToJSString AlignEnd    = JS.pack "end"
textAlignToJSString AlignLeft   = JS.pack "left"
textAlignToJSString AlignCenter = JS.pack "center"
textAlignToJSString AlignRight  = JS.pack "right"

setTextAlign :: (MonadIO m) => JSContext2D -> TextAlign -> m ()
setTextAlign ctx align = liftIO $ js_textAlign ctx (textAlignToJSString align)

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

-- * Various Transformations

foreign import javascript unsafe "$1[\"scale\"]($2, $3)"
  js_scale :: JSContext2D -> Double -> Double -> IO ()

foreign import javascript unsafe "alert($1)"
  js_alert :: JSString -> IO ()

scale :: (MonadIO m) => JSContext2D -> Double -> Double -> m ()
scale ctx x y = liftIO $ js_scale ctx x y

foreign import javascript unsafe "$1[\"rotate\"]($2)"
  js_rotate :: JSContext2D -> Double -> IO ()

-- | apply rotation to commands that draw on the canvas
rotate :: (MonadIO m) =>
         JSContext2D -- ^ canvas to affect
      -> Double -- ^ rotation in radians
      -> m ()
rotate ctx r = liftIO $ js_rotate ctx r

foreign import javascript unsafe "$1[\"translate\"]($2, $3)"
  js_translate :: JSContext2D -> Double -> Double -> IO ()

-- | apply translation to commands that draw on the canvas
translate :: (MonadIO m) =>
             JSContext2D -- ^ canvas
          -> Double -- ^ x translation
          -> Double -- ^ y translation
          -> m ()
translate ctx x y = liftIO $ js_translate ctx x y

data Gradient = Gradient
    deriving (Eq, Show, Read)
data Pattern = Pattern
    deriving (Eq, Show, Read)

type Percentage = Double
type Alpha = Double

data Color
  = ColorName JSString
  | RGBA Percentage Percentage Percentage Alpha
    deriving (Eq, Show, Read)

data Style
  = StyleColor Color
  | StyleGradient Gradient
  | StylePattern Pattern
    deriving (Eq, Show, Read)

data Rect
  = Rect { _rectX      :: Double
         , _rectY      :: Double
         , _rectWidth  :: Double
         , _rectHeight :: Double
         }
  deriving (Eq, Show, Read)

-- https://developer.mozilla.org/en-US/docs/Web/API/Path2D
data Path2D
  = MoveTo Double Double
  | LineTo Double Double
  | PathRect Rect
  | Arc Double Double Double Double Double Bool
    deriving (Eq, Show, Read)

data Draw
  = FillRect Rect
  | ClearRect Rect
  | Stroke [Path2D]
  | Fill [Path2D]
  | FillText JSString Double Double (Maybe Double)
    deriving (Eq, Show, Read)

data Context2D
  = FillStyle Style
  | StrokeStyle Style
  | LineWidth Double
  | Font JSString
  | TextAlign TextAlign
  | Scale Double Double
  | Translate Double Double
  | Rotate Double
    deriving (Eq, Read, Show)

-- | this is not sustainable. A Set of attributes is probably a better choice

data Canvas = Canvas
  { _canvasId :: Text
  , _canvas :: Canvas2D
  }
  deriving (Eq, Show, Read)

data Canvas2D
  = WithContext2D [Context2D] [ Canvas2D ]
  | Draw Draw
  deriving (Eq, Show, Read)

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
           -- http://www.html5rocks.com/en/tutorials/canvas/hidpi/
           -- NOTE: backingStorePixelRatio is deprecated, we just ignore it
        do rescaleCanvas <- do ms <- getData canvasElem (JS.pack "rescale")
                               case ms of
                                Nothing -> pure True
                                (Just s) ->
                                  case (JS.unpack s) of
                                    "true" -> pure True
                                    _ -> pure False
           ratio <- fmap (fromMaybe 1) (devicePixelRatio =<< window)
           (w, h) <-
              if rescaleCanvas
                 then do (Just oldWidth)  <- fmap (read . JS.unpack) <$> getAttribute canvasElem (JS.pack "width")
                         (Just oldHeight) <- fmap (read . JS.unpack) <$> getAttribute canvasElem (JS.pack "height")
                         js_setAttribute canvasElem (JS.pack "width")  (JS.pack $ show $ oldWidth * ratio)
                         js_setAttribute canvasElem (JS.pack "height") (JS.pack $ show $ oldHeight * ratio)
                         setStyle canvasElem (JS.pack "width")  (JS.pack  $ (show oldWidth) ++ "px")
                         setStyle canvasElem (JS.pack "height")  (JS.pack $ (show oldHeight) ++ "px")
                         setData canvasElem (JS.pack "rescale") (JS.pack "false")
                         pure (oldWidth * ratio, oldHeight * ratio)
                 else do (Just width)  <- fmap (read . JS.unpack) <$> getAttribute canvasElem (JS.pack "width")
                         (Just height) <- fmap (read . JS.unpack) <$> getAttribute canvasElem (JS.pack "height")
                         pure (width, height)
           mctx <- getContext2D canvasElem
           case mctx of
            Nothing -> pure ()
            (Just ctx) -> do
              when (rescaleCanvas) (scale ctx ratio ratio)
              clearRect ctx 0 0 w h
              drawCanvas' ctx content
  where
    drawCanvas' ctx (Draw (FillRect (Rect x y w h))) =
      fillRect ctx x y w h
    drawCanvas' ctx  (Draw (ClearRect (Rect x y w h))) =
      clearRect ctx x y w h
    drawCanvas' ctx  (Draw (Stroke path2D)) =
      do mkPath ctx path2D
         stroke ctx
    drawCanvas' ctx  (Draw (Fill path2D)) =
      do mkPath ctx path2D
         fill ctx
    drawCanvas' ctx  (Draw (FillText text x y maxWidth)) =
      do fillText ctx text x y maxWidth
    drawCanvas' ctx  (WithContext2D ctx2d content) =
     do save ctx
        mapM_ (setContext2D ctx) ctx2d
        mapM_ (drawCanvas' ctx) content
        restore ctx
        where
          setContext2D ctx op =
            case op of
             (FillStyle style)   -> setFillStyle ctx style
             (StrokeStyle style) -> setStrokeStyle ctx style
             (LineWidth w)       -> setLineWidth ctx w
             (Font font)         -> setFont ctx font
             (TextAlign a)       -> setTextAlign ctx a
             (Scale x y)         -> scale ctx x y
             (Translate x y)     -> translate ctx x y
             (Rotate r )         -> rotate ctx r

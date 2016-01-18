{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.ISO.Murv where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Data.JSString.Text (textToJSString)
import GHCJS.Foreign (jsNull)
import GHCJS.Foreign.Callback (asyncCallback1)
import GHCJS.Types (JSString(..))
import Web.ISO.Diff
import Web.ISO.Patch
import Web.ISO.Types

--   | Children [HTML action]
{-
flattenHTML :: HTML action -> HTML action
flattenHTML h@(CDATA _ _) = h
flattenHTML h@(Children _) = h
flattenHTML (Element t acts attrs children)
-}
{-
renderHTML :: forall action m. (MonadIO m) => (action -> IO ()) -> JSDocument -> HTML action -> m (Maybe JSNode)
renderHTML _ doc (CDATA _ t) = fmap (fmap toJSNode) $ createJSTextNode doc t
renderHTML handle doc (Element tag {- events -} attrs _ children) =
    do me <- createJSElement doc tag
       case me of
         Nothing -> return Nothing
         (Just e) ->
             do mapM_ (\c -> appendJSChild e =<< renderHTML handle doc c) children
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
-}
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
data MURV  model action remote = MURV
    { model  :: model
    , update :: action -> model -> (model, Maybe remote)
    , view   :: model  -> (HTML action, [Canvas])
    }

mainLoopRemote :: (Show action) => Text -> (Text -> action) -> JSDocument -> JSNode -> MURV model action Text -> Maybe action -> IO ()
mainLoopRemote url h document body (MURV model update view) mInitAction =
    do queue <- atomically newTQueue
       let (vdom, canvases) = view model
       -- update HTML
       html <- renderHTML (handleAction queue) document vdom
       removeChildren body
       appendChild body html
       -- update Canvases
       mapM_ drawCanvas canvases
       -- xhr request
       xhr <- newXMLHttpRequest
--       cb <- asyncCallback1 (\_ -> handleXHR queue xhr)
       addEventListener xhr ReadyStateChange (\_ -> handleXHR queue xhr) False
--       remoteLoop queue xhr
       case mInitAction of
         (Just initAction) ->
             handleAction queue initAction
         Nothing -> return ()
       loop xhr queue model vdom
    where
      handleXHR queue xhr =
          do t <- getResponseText xhr
             atomically $ writeTQueue queue (h t)
      handleAction queue = \action -> atomically $ writeTQueue queue action
--      remoteLoop queue xhr = forkIO $
--          return ()
      loop xhr queue model oldVDom =
          do action <- atomically $ readTQueue queue
             let (model', mremote') = update action model
             let (vdom, canvases) = view model'
                 diffs = diff oldVDom (Just vdom)
--             putStrLn $ "action --> " ++ show action
--             putStrLn $ "diff --> " ++ show diffs
             -- update HTML
             apply (handleAction queue) document body oldVDom diffs
             -- update Canvases
             mapM_ drawCanvas canvases
--             html <- renderHTML (handleAction queue) document vdom
--             removeChildren body
--             appendJSChild body html
             case mremote' of
               Nothing -> return ()
               (Just remote) ->
                   do open xhr "POST" url True
                      sendString xhr (textToJSString remote)
             loop xhr queue model' vdom

murv :: (Show action) =>
        Text                     -- ^ remote API URL
     -> (Text -> action)         -- ^ convert a remote response to an 'action'
     -> MURV model action Text -- ^ model-update-remote-view record
     -> (Maybe action)           -- ^ initial action
     -> IO ()
murv url h m initAction =
    do (Just document) <- currentDocument
       murv <- do mmurv <- getElementById document "murv"
                  case mmurv of
                    (Just murv) -> return (toJSNode murv)
                    Nothing ->
                        do (Just bodyList) <- getElementsByTagName document "body"
                           (Just body)     <- item bodyList 0
                           return body
       mainLoopRemote url h document murv m initAction

{-# LANGUAGE ScopedTypeVariables #-}
module Web.ISO.Murv where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import GHCJS.Foreign (ToJSString(..), FromJSString(..), ForeignRetention(AlwaysRetain), asyncCallback, jsNull)
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
    , view   :: model  -> HTML action
    }

mainLoopRemote :: (ToJSString remote, Show action) => Text -> (Text -> action) -> JSDocument -> JSNode -> MURV model action remote -> IO ()
mainLoopRemote url h document body (MURV model update view) =
    do queue <- atomically newTQueue
       let vdom = view model
       html <- renderHTML (handleAction queue) document vdom
       removeChildren body
       appendJSChild body html
       xhr <- newXMLHttpRequest
       cb <- asyncCallback AlwaysRetain (handleXHR queue xhr)
       addEventListener xhr "readystatechange" cb False
--       remoteLoop queue xhr
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
             let vdom = view model'
                 diffs = diff oldVDom vdom
             putStrLn $ "action --> " ++ show action
             putStrLn $ "diff --> " ++ show diffs
             apply document body vdom diffs
--             html <- renderHTML (handleAction queue) document vdom
--             removeChildren body
--             appendJSChild body html
             case mremote' of
               Nothing -> return ()
               (Just remote) ->
                   do open xhr "POST" url True
                      sendString xhr (toJSString remote)
             loop xhr queue model' vdom

murv :: (ToJSString remote, Show action) => Text -> (Text -> action) -> MURV model action remote -> IO ()
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

{-# LANGUAGE ScopedTypeVariables #-}
{- Apply some patches -}
module Web.ISO.Patch where

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (unpack)
import GHCJS.Types (castRef)
import Web.ISO.Diff (Patch(..))
import Web.ISO.Types (HTML(..), Attr(..), JSDocument, JSElement(..), JSNode, childNodes, item, getFirstChild, getLength, replaceData, setAttribute, unJSNode, setValue)

-- data GetNodeState
{-
-- | get the list of specified nodes from the DOM
getNodes :: forall action. HTML action -- ^ virtual DOM that matches the current DOMe
         -> JSNode -- ^ root node of DOM
         -> [Int] -- ^ nodes indices we want (using in-order numbering)
         -> IO [(Int, JSNode)]
getNodes html nodeIndices =
    evalState (getNodes' html nodeIndices) 0
    where
      getNodes' :: HTML action -> [Int] -> IO [(Int, JSNode)]
      getNodes' _ [] = return []
      getNodes' node@(CData _ _) (i:is) =
          do index <- get
             if (
-}

apply :: JSDocument -> JSNode -> HTML action -> Map Int [Patch action] -> IO JSNode
apply document rootNode vdom patches =
    do let keys = Map.keys patches
       putStrLn $ "apply = " ++ show patches
       (Just first) <- getFirstChild rootNode
       nodeList <- getNodes first vdom keys
       putStrLn $ "nodeList length = " ++ show (length nodeList)
       mapM_ (apply' patches) nodeList
       return rootNode

apply' :: Map Int [Patch action] -> (Int, JSNode) -> IO ()
apply' patchMap (index, node) =
    case Map.lookup index patchMap of
      (Just patches) ->
          mapM_ (apply'' node) patches
      Nothing -> error $ "Y NO PATCH? " ++ show index

apply'' :: JSNode
        -> Patch action
        -> IO ()
apply'' node patch =
    case patch of
      (VText b t) -> do oldLength <- getLength node
                        putStrLn $  "replaceData(0" ++ ", " ++ show oldLength ++ ", " ++ unpack t ++ ")"
                        replaceData node 0 oldLength (escape b t)
      (Props newProps) -> -- FIXME: doesn't handle changes to events.
          do let e = JSElement $ castRef $ unJSNode node
             putStrLn $ "set properties: " ++ show [ (k,v) | Attr k v <- newProps ]
             mapM_ (\(k, v) ->
                        case (unpack k) of
                          "value" -> setValue e v
                          _ -> setAttribute e k v) [ (k,v) | Attr k v <- newProps ]
      _ -> return ()

escape _ t = t


-- FIXME: do not walk down DOM trees that contain no nodes if interest
getNodes :: forall action.
            JSNode -- ^ root node of DOM
         -> HTML action -- ^ virtual DOM that matches the current DOMe
         -> [Int] -- ^ nodes indices we want (using in-order numbering)
         -> IO [(Int, JSNode)]
getNodes currNode vdom nodeIndices =
    evalStateT (getNodes' currNode vdom nodeIndices) 0
    where
      inc :: (MonadIO m) => StateT Int m Int
      inc =
          do i <- get
             let i' = i + 1
             put i'
             return i'

      getNodes' :: JSNode
                -> HTML action
                -> [Int]
                -> StateT Int IO [(Int, JSNode)]
      getNodes' _ _ [] = return []
      getNodes' currNode node@(CDATA _ _) (i:_) =
          do index <- get
             liftIO $ putStrLn $ "CDATA index = " ++ show index
             if (i == index)
                then do liftIO $ putStrLn $ "match CDATA on index = " ++ show index
                        return [(i, currNode)]
                else return [] -- error $ "End of the road. But i /= index. " ++ show (i, index)
--      getNodes' currNode node@(CDATA _ _) is =
--          error $ "Got CDATA but multiple indices, " ++ show is
      getNodes' currNode vdom@(Element _tag _attrs count children) is'' =
          do index <- get
             case dropWhile (\i -> i < index) is'' of
               [] -> return []
               (i:is) ->
                   do liftIO $ putStrLn $ "Element index = " ++ show index ++ " looking for i = " ++ show i
                      cs <- childNodes currNode
                      l <- fromIntegral <$> getLength cs
                      liftIO $ putStrLn $ "l = "    ++ show l
                      liftIO $ putStrLn $ "vdom = " ++ show vdom
                      let is' = if (i == index) then is else (i:is)
                      childNodes' <- mapM (\i' -> do (Just c) <- item cs (fromIntegral i')
                                                     liftIO $ putStrLn $ "i' = " ++ show i'
                                                     inc
                                                     getNodes' c (children!!i') is'
                                          ) [0..(l-1)]
                      if (i == index)
                      then do liftIO $ putStrLn $ "match Element on index = " ++ show index
                              return $ (i, currNode) : (concat childNodes')
                      else return (concat childNodes')

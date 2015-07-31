{-# LANGUAGE ScopedTypeVariables #-}
{- Apply some patches -}
module Web.ISO.Patch where

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Web.ISO.Diff (Patch(..))
import Web.ISO.Types (HTML(..), Attr(..), JSDocument, JSNode, childNodes, item, getFirstChild, setTextContent, getLength)

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
       (Just first) <- getFirstChild rootNode
       nodeMap <- getNodes first vdom keys
       mapM_ (apply' patches) nodeMap
       return rootNode

apply' :: Map Int [Patch action] -> (Int, JSNode) -> IO ()
apply' patchMap (index, node) =
    case Map.lookup index patchMap of
      (Just patches) ->
          mapM_ (apply'' node) patches
      Nothing -> error $ "Y NO PATCH? " ++ show index

apply'' :: JSNode -> Patch action -> IO ()
apply'' node patch =
    case patch of
      (VText b t) -> do putStrLn $  "textContent = " ++ show t
                        setTextContent node (escape b t)
      _ -> return ()

escape _ t = t

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
             if (i == index)
                then return [(i, currNode)]
                else return [] -- error $ "End of the road. But i /= index. " ++ show (i, index)
--      getNodes' currNode node@(CDATA _ _) is =
--          error $ "Got CDATA but multiple indices, " ++ show is
      getNodes' currNode vdom@(Element _tag _attrs count children) (i:is) =
          do index <- get
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
              then return $ (i, currNode) : (concat childNodes')
              else return (concat childNodes')

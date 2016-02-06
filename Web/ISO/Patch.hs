{-# LANGUAGE ScopedTypeVariables #-}
{- Apply some patches -}
module Web.ISO.Patch where

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO(..))
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (unpack)
import Web.ISO.Diff (Patch(..))
import Web.ISO.Types (HTML(..), Attr(..), JSDocument, JSElement(..), JSNode, childNodes, item, getFirstChild, getLength, replaceData, setAttribute, unJSNode, setValue, parentNode, removeChild, replaceChild, renderHTML, appendChild)

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

apply :: (action -> IO ())
      -> JSDocument
      -> JSNode
      -> HTML action
      -> Map Int [Patch action]
      -> IO JSNode
apply handle document rootNode vdom patches =
    do let indices = Map.keys patches
       case indices of
        [] -> pure rootNode
        _ -> do putStrLn $ "indices (keys) = " ++ show indices
                putStrLn $ "apply = " ++ show patches
                (Just first) <- getFirstChild rootNode -- FIXME: handle Nothing
                nodeList <- getNodes first vdom indices
                putStrLn $ "nodeList length = " ++ show (length nodeList)
                mapM_ (apply' handle document patches) nodeList
                return rootNode

apply' :: (action -> IO ())
       -> JSDocument
       -> Map Int [Patch action]
       -> (Int, JSNode)
       -> IO ()
apply' handle document patchMap (index, node) = do
    putStrLn $ "apply' with index = " ++ show index
    case Map.lookup index patchMap of
      (Just patches) ->
          mapM_ (apply'' handle document node) patches
      Nothing -> error $ "Y NO PATCH? " ++ show index

apply'' :: (action -> IO ())
        -> JSDocument
        -> JSNode
        -> Patch action
        -> IO ()
apply'' handle document node patch =
    case patch of
      (VText b t) -> do oldLength <- getLength node
                        putStrLn $  "replaceData(0" ++ ", " ++ show oldLength ++ ", " ++ unpack t ++ ")"
                        replaceData node 0 oldLength (escape b t)
      (Props newProps) -> -- FIXME: doesn't handle changes to events.
          do let e = JSElement $ unJSNode node
--             putStrLn $ "set properties: " ++ show [ (k,v) | Attr k v <- newProps ]
             mapM_ (\(k, v) ->
                        case (unpack k) of
--                          "value" -> setValue e v -- FIXME: this causes issues with the cursor position
                          _ -> setAttribute e k v) [ (k,v) | Attr k v <- newProps ]
      (Insert elem) ->
          -- FIXME: don't get parent?
          do mparent <- parentNode node
             case mparent of
               Nothing -> putStrLn $ "Can't appendChild because there is no parentNode"
               (Just parent) ->
                   do putStrLn $  "Insert --> " ++ show elem
                      child <- renderHTML handle document elem
                      appendChild node child
                      return ()
      Remove ->
          do mparent <- parentNode node
             case mparent of
               Nothing -> putStrLn $ "Can't removeChild because there is no parentNode"
               (Just parent) ->
                   do removeChild parent (Just node)
                      return ()
      VNode newElem ->
          do mparent <- parentNode node
             case mparent of
               Nothing ->  putStrLn $ "Can't replaceChild because there is no parentNode"
               (Just parent) ->
                   do (Just newChild) <- renderHTML handle document newElem
                      replaceChild parent newChild node
                      return ()
{-
      p -> do putStrLn $ "Skipping patch " ++ show patch
              return ()
-}
escape _ t = t


-- FIXME: do not walk down DOM trees that contain no nodes if interest
getNodes :: forall action.
            JSNode      -- ^ root node of DOM
         -> HTML action -- ^ virtual DOM that matches the current DOM
         -> [Int]       -- ^ nodes indices we want (using in-order numbering)
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
--             liftIO $ putStrLn $ "CDATA index = " ++ show index
             if (i == index)
                then do -- liftIO $ putStrLn $ "match CDATA on index = " ++ show index
                        return [(i, currNode)]
                else return [] -- error $ "End of the road. But i /= index. " ++ show (i, index)
--      getNodes' currNode node@(CDATA _ _) is =
--          error $ "Got CDATA but multiple indices, " ++ show is
      getNodes' currNode vdom@(Element _tag _attrs _key count children) is'' =
          do index <- get
             case getInRange index count is'' of
               [] -> return []
               (i:is) ->
                   do -- liftIO $ putStrLn $ "Element index = " ++ show index ++ " looking for i = " ++ show i
                      cs <- childNodes currNode
                      l <- fromIntegral <$> getLength cs
--                      liftIO $ putStrLn $ "l = "    ++ show l
--                      liftIO $ putStrLn $ "vdom = " ++ show vdom
                      let is' = if (i == index) then is else (i:is)
                      childNodes' <- mapM (\i' -> do (Just c) <- item cs (fromIntegral i')
                                                     -- liftIO $ putStrLn $ "l = " ++ show l ++ ", length children = " ++ show (length children) ++ ", i' = " ++ show i'
                                                     inc
                                                     getNodes' c (children!!i') is'
                                          ) [0..(l-1)]
                      if (i == index)
                      then do -- liftIO $ putStrLn $ "match Element on index = " ++ show index
                              return $ (i, currNode) : (concat childNodes')
                      else return (concat childNodes')
      getInRange index count indexes =
          {- takeWhile (\i -> i <= index + count) $ -} dropWhile (\i -> i < index) indexes

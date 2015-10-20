{-# LANGUAGE ScopedTypeVariables #-}
module Web.ISO.Diff where

import Control.Monad.State (State(..), evalState, get, put)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Web.ISO.Types (HTML(..), Attr(..), descendants)

data Patch action
    = Remove
    | Insert (HTML action)
    | VText Bool Text
    | VNode (HTML action)
    | Props [Attr action] -- FIXME: add list of attributes to remove

instance Show (Patch action) where
    show Remove = "Remove"
    show (Insert node) = "Insert " <> show node
    show (VText b t) = "VText " <> show b <> " " <> Text.unpack t
    show (VNode e)  = "VNode " <> show e
    show (Props attrs) = "Props " <> show attrs


diff :: forall action. HTML action -> HTML action -> Map Int [Patch action]
diff a b = Map.fromListWith (++) (evalState (diff' a b) 0)
    where
      inc :: State Int Int
      inc =
          do i <- get
             let i' = i + 1
             put i'
             return i'

      diff' :: HTML action -> HTML action -> State Int [(Int, [Patch action])]
      diff' (Element tagNameA attrsA descendantsA childrenA) b@(Element tagNameB attrsB _ childrenB)
          | (tagNameA /= tagNameB) || (diffIds attrsA attrsB) =
              do index <- get
                 put (index + descendantsA)
                 return [(index, [VNode b])]
          | otherwise =
              do propsPatches    <- diffAttrs attrsA attrsB
                 index <- get
                 childrenPatches <- diffChildren index childrenA childrenB
                 return $ propsPatches ++ childrenPatches
          where
            findAttrVal :: Text -> [Attr action] -> Maybe Text
            findAttrVal _ [] = Nothing
            findAttrVal n' (Attr n v : _)
                | n' == n = Just v
            findAttrVal n (_ : as) = findAttrVal n as

            diffIds :: [Attr action] -> [Attr action] -> Bool
            diffIds attrsA attsrB = (findAttrVal (pack "id") attrsA) /= (findAttrVal (pack "id") attrsB)

      diff' (CDATA escapeA txtA) b@(CDATA escapeB txtB)
            | escapeA == escapeB && txtA == txtB =
                return []
            | otherwise =
                do index <- get
                   return [(index, [VText escapeB txtB])]
      diff' (Element _tagNameA _attrsA descendantsA _childrenA) b =
          do index <- get
             put (index + descendantsA)
             return [(index, [VNode b])] -- FIXME: this does not work correctly if the node is not the last in the list of children
                                                 -- FIXME: maybe we want VNode?
      diff' (CDATA{}) b =
          do index <- get
             return [(index, [VNode b])] -- FIXME: this does not work correctly if the node is not the last in the list of children
                                                 -- FIXME: maybe we want VNode?
      -- FIXME: does not handle changes to Events
      -- FIXME: we should be able to add and remove single attributes
      diffAttrs :: [Attr action] -> [Attr action] -> State Int [(Int, [Patch action])]
      diffAttrs attrsA attrsB =
          let attrsA' = [(k,v) | Attr k v <- attrsA]
              attrsB' = [(k,v) | Attr k v <- attrsB]
          in if attrsA' == attrsB'
             then return []
             else do index <- get
                     return [(index, [Props attrsB])]
      -- FIXME: handle reordered children
      diffChildren :: Int -> [HTML action] -> [HTML action] -> State Int [(Int, [Patch action])]
      diffChildren _ [] [] = return []
      diffChildren pid (a:as) (b:bs) =
          do index <- inc
             d <- diff' a b
             diffs <- diffChildren pid as bs
             return $ d ++ diffs
      diffChildren pid (a:as) [] =
          do index <- inc
             put (index + descendants [a])
             diffs <- diffChildren pid as []
             return $ (index, [Remove]) : diffs
      diffChildren pid [] cs =
          do index <- get
             return $ [(pid, map Insert cs)]

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
      deriving Eq

instance Show (Patch action) where
    show Remove        = "Remove"
    show (Insert node) = "Insert " <> show node
    show (VText b t)   = "VText " <> show b <> " " <> Text.unpack t
    show (VNode e)     = "VNode " <> show e
    show (Props attrs) = "Props " <> show attrs

diff :: forall action. HTML action -> Maybe (HTML action) -> Map Int [Patch action]
diff a b = Map.fromListWith (++) (walk a b 0)

-- FIXME: does not handle changes to Events
-- FIXME: we should be able to add and remove single attributes
diffAttrs :: [Attr action] -> [Attr action] -> Int -> [(Int, [Patch action])]
diffAttrs attrsA attrsB index =
          let attrsA' = [(k,v) | Attr k v <- attrsA]
              attrsB' = [(k,v) | Attr k v <- attrsB]
          in if attrsA' == attrsB'
             then []
             else [(index, [Props attrsB])]

walk :: HTML action -> Maybe (HTML action) -> Int -> [(Int, [Patch action])]
walk a mb index =
  case mb of
   Nothing -> [(index, [Remove])]
   (Just b@(Element tagNameB attrsB keyB _ childrenB)) ->
     case a of
      (Element tagNameA attrsA keyA descendantsA childrenA)
        | tagNameA == tagNameB && keyA == keyB ->
            let propsPatches    = diffAttrs attrsA attrsB index
                childrenPatches = diffChildren index childrenA childrenB index
            in propsPatches ++ childrenPatches
      _ -> [(index, [VNode b])]
   (Just (CDATA escapeB txtB)) ->
     case a of
      (CDATA escapeA txtA)
        | escapeA == escapeB && txtA == txtB -> []
      _ -> [(index, [VText escapeB txtB])]

diffChildren :: Int -> [HTML action] -> [HTML action] -> Int -> [(Int, [Patch action])]
diffChildren parentIndex childrenA childrenB index =
  case (childrenA, childrenB) of
   ([], []) -> []
   ([], (b:bs)) ->
     (parentIndex, [Insert b]) : diffChildren parentIndex [] bs (index + 1)
   ((a:as), []) ->
     (walk a Nothing (index + 1)) ++ (diffChildren parentIndex as [] (index + 1 + (elementDescendants' a)))
   ((a:as), (b:bs)) ->
     (walk a (Just b) (index + 1)) ++ (diffChildren parentIndex as bs (index + 1 + (elementDescendants' a)))
  where
       elementDescendants' (CDATA {}) = 0
       elementDescendants' e = elementDescendants e
{-
diffChildren [] cs index = [(pid, map Insert cs)]
diffChildren (a:as) (b:bs) index =
   let d = walk a b (index + 1)
       diffs = diffChildren pid as bs
       return $ d ++ diffs
diffChildren (a:as) [] index =
  do diffs <- diffChildren pid as []
     return $ (index, [Remove]) : diffs
-}
{-
      walk (Element tagNameA attrsA keyA descendantsA childrenA) b@(Element tagNameB attrsB keyB _ childrenB)

      // two cdata that are the same
      walk (CDATA escapeA txtA) b@(CDATA escapeB txtB)
            | escapeA == escapeB && txtA == txtB =
                return []
            | otherwise =
                do index <- get
                   return [(index, [VText escapeB txtB])]
      walk (Element _tagNameA _attrsA descendantsA _childrenA) b =
          do index <- get
             put (index + descendantsA)
             return [(index, [VNode b])] -- FIXME: this does not work correctly if the node is not the last in the list of children
                                                 -- FIXME: maybe we want VNode?
      walk (CDATA{}) b =
          do index <- get
             return [(index, [VNode b])] -- FIXME: this does not work correctly if the node is not the last in the list of children
                                                 -- FIXME: maybe we want VNode?
      -- FIXME: handle reordered children
      diffChildren :: Int -> [HTML action] -> [HTML action] -> State Int [(Int, [Patch action])]
      diffChildren _ [] [] = return []
      diffChildren pid (a:as) (b:bs) =
          do index <- inc
             d <- walk a b
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
-}
{-
          where
            findAttrVal :: Text -> [Attr action] -> Maybe Text
            findAttrVal _ [] = Nothing
            findAttrVal n' (Attr n v : _)
                | n' == n = Just v
            findAttrVal n (_ : as) = findAttrVal n as

            diffIds :: [Attr action] -> [Attr action] -> Bool
            diffIds attrsA attsrB = (findAttrVal (pack "id") attrsA) /= (findAttrVal (pack "id") attrsB)
-}

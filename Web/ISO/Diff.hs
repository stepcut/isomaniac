{-# LANGUAGE ScopedTypeVariables #-}
module Web.ISO.Diff where

import Control.Monad.State (State(..), evalState, get, put)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Web.ISO.Types (HTML(..), Attr(..))

data Patch action
    = Remove
    | Insert (HTML action)
    | VText Bool Text
    | VNode (HTML action)
    | Props [Attr action]

instance Show (Patch action) where
    show Remove = "Remove"
    show (Insert node) = "Insert " <> show node
    show (VText b t) = "VText " <> show b <> " " <> Text.unpack t
    show (VNode e)  = "VNode " <> show e
    show (Props attrs) = "Props " <> show attrs


diff :: forall action. HTML action -> HTML action -> Map Int [Patch action]
diff a b = Map.fromAscListWith (++) (evalState (diff' a b) 0)
    where
      inc :: State Int Int
      inc =
          do i <- get
             let i' = i + 1
             put i'
             return i'

      diff' :: HTML action -> HTML action -> State Int [(Int, [Patch action])]
      diff' (Element tagNameA attrsA _ childrenA) b@(Element tagNameB attrsB _ childrenB)
          | tagNameA /= tagNameB =
              do index <- get
                 return [(index, [VNode b])]
          | otherwise =
              do propsPatches    <- diffAttrs attrsA attrsB
                 childrenPatches <- diffChildren childrenA childrenB
                 return $ propsPatches ++ childrenPatches
      diff' (CDATA escapeA txtA) b@(CDATA escapeB txtB)
            | escapeA == escapeB && txtA == txtB =
                return []
            | otherwise =
                do index <- get
                   return [(index, [VText escapeB txtB])]
      diff' _ b =
          do index <- get
             return [(index, [Remove, VNode b])] -- FIXME: this does not work correctly if the node is not the last in the list of children
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
      diffChildren :: [HTML action] -> [HTML action] -> State Int [(Int, [Patch action])]
      diffChildren [] [] = return []
      diffChildren (a:as) (b:bs) =
          do d <- diff' a b
             index <- inc
             diffs <- diffChildren as bs
             return $ d ++ diffs
      diffChildren (a:as) [] =
          do index <- inc
             diffs <- diffChildren as []
             return $ (index, [Remove]) : diffs
      diffChildren [] cs =
          do index <- get
             return $ [(index, map Insert cs)]

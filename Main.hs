module Main where

import Types

main :: IO ()
main =
    do (Just document) <- currentDocument
       (Just bodyList) <- getElementsByTagName document "body"
       (Just body) <- item bodyList 0
       tn <- createJSTextNode document "Hello, GHCJS!"
       (Just e)  <- createJSElement  document "div"
       appendJSChild e tn
       appendJSChild body (Just e)
       return ()

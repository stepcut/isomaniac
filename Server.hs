module Main where

import Control.Monad (msum)
import qualified Data.ByteString.Char8 as B
import Happstack.Server

main :: IO ()
main =
    simpleHTTP nullConf handler

handler = msum
    [ dir "api" $ do
        method POST
        rq <- askRq
        (Just (Body s)) <- takeRequestBody rq
        ok $ toResponseBS (B.pack "text/plain") s
    , serveDirectory EnableBrowsing [] "Main.jsexe"
    ]

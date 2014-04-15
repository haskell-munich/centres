
module Main where

import Point
import ConvexHull (convexHull)
import Perceptron

import Happstack.Server
import qualified Data.ByteString.Lazy.Char8 as B


import Data.Aeson (decode, encode)

import qualified Data.List as List
import Data.Maybe (catMaybes)

import Control.Monad.IO.Class
import Control.Monad

setMime :: String -> B.ByteString -> Response
setMime mt txt =
  (toResponse "")
    { rsBody=txt,
      rsHeaders=(mkHeaders [("Content-Type", mt)])} 


centre :: [Point] -> Maybe Point
centre [] = Nothing
centre ps@(p:_) = Just $ d (List.foldl' f zero ps) (length ps)
  where zero = Point 0 0 (colour p)
        d p l = p { x = x p `div` fromIntegral l,
                    y = y p `div` fromIntegral l }

        f acc (Point a b _) = acc { x = x acc + a, y = y acc + b }

listHandler :: ServerPart Response
listHandler = do
  cmd <- look "cmd"
  lst <- look "vertices"

  liftIO (print cmd)
  liftIO (print lst)

  let pts :: [Point]
      Just pts = decode (B.pack lst)

      (rs, gs) = partition ((Red ==) . colour) pts

      centres = catMaybes [centre rs, centre gs]

      cvhs = [convexHull rs, convexHull gs]

      line = perceptron pts

  liftIO (print pts)
  liftIO (print line)

  case cmd of
       "centres" -> ok (toResponse (encode centres))
       "convexhull" -> ok (toResponse (encode cvhs))
       "perceptron" -> ok (toResponse (encode line))

main :: IO ()
main = do
  d3 <-  B.readFile "d3/d3.v3.min.js"
  jq <-  B.readFile "jquery/jquery-1.11.0.min.js"

  simpleHTTP nullConf $ do
    canv <-liftIO $ B.readFile "js/canvas.js"

    txt <- liftIO $ B.readFile "html/index.html"
    liftIO (putStrLn "Hi there!")
    msum [ dir "index.html" $ return (setMime "text/html" txt),
           dir "d3.v3.min.js" $ ok (setMime "text/javascript" d3),
           dir "jquery-1.11.0.min.js" $ ok (setMime "text/javascript" jq),
           dir "js" $ serveDirectory DisableBrowsing [] "js",
           dir "css" $ serveDirectory DisableBrowsing [] "css",
           listHandler ]

{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Control.Arrow
import Pickler.Pickle
import Numeric
import Prelude

data URL = URL { protocol :: String,
                 host :: String,
                 port :: Maybe Int,
                 file :: String} deriving(Show)
url = wrap (\x -> (protocol x, host x, port x, file x),
            \(pr,h,pt,f) -> URL {protocol = pr, host=h, port = pt, file = f} )
            (quad string string (pMaybe nat) string)
            
data Bookmark = Link (String, URL)  | Folder (String, [Bookmark]) deriving(Show)
bookmark =
    alt tag [ wrap (\(Link(s,u)) -> (s,u), Link )
                       (pair string url) ,
              wrap (\(Folder(s,bs)) -> (s,bs), Folder)
                    (pair string (list bookmark)) ]
           where tag (Link _) = 0; tag (Folder _) = 1

type Bookmarks = [Bookmark] 
bookmarks = list bookmark
            
x = [ Link("Andrew", URL {protocol = "http",
                          host = "research.microsoft.com",
                          port = Nothing,
                          file = "users/akenn" })]
px = pickle bookmarks x
x' = unpickle bookmarks px
s = map (\c -> showHex (fromEnum c) "")  px
              
main                    :: IO ()
main                    =  do print s
                              putStrLn "Hi"
                              print x'

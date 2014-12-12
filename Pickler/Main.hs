import System.Environment
import Pickler.Pickle
import Prelude

data URL = URL { protocol :: String,
                 host :: String,
                 port :: Maybe Int,
                 file :: String}
url = wrap (\x -> (protocol x, host x, port x, file x),
            \(pr,h,pt,f) -> URL {protocol = pr, host=h, port = pt, file = f} )
            (quad string string (pMaybe nat) string)
            
data Bookmark = Link (String, URL)  | Folder (String, [Bookmark])

x = [ Link("Andrew", URL {protocol = "http",
                          host = "research.microsoft.com",
                          port = Nothing,
                          file = "users/akenn" })]
              
main                    :: IO ()
main                    =  do c <- getChar
                              putChar c

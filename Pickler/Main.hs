import System.Environment
import Pickler.Pickle
import Prelude

data URL = URL { protocol :: String,
                 host :: String,
                 port :: Maybe Int,
                 file :: String}
data Bookmark = Link (String, URL)  | Folder (String, [Bookmark])

x = [ Link("Andrew", URL {protocol = "http",
                          host = "research.microsoft.com",
                          port = Nothing,
                          file = "users/akenn" })]
              
main                    :: IO ()
main                    =  do c <- getChar
                              putChar c

module Pretty.Pretty  (Doc, (<>), nil,text,line,nest,layout)  where

import Prelude

(<>) ::Doc->Doc->Doc 
nil :: Doc
text :: String -> Doc 
line :: Doc
nest ::Int->Doc->Doc 
layout :: Doc -> String


map':: (a -> a) -> [a] -> [a]
map'  _ [] = []
map'  fother (x:xs) =  x : map fother xs


--this algebraic type represent every normal form
--of document, so it can represent every document
data Doc = Nil 
         | String  `Text` Doc  -- = text s <> x
         | Int `Line` Doc

nil = Nil
text s = s `Text` Nil
line   = 0 `Line` Nil

(s `Text` x) <> y   = undefined 
(i `Line` x) <> y = undefined
Nil <> y = undefined


nest i (s `Text` x)    = undefined 
nest i (j `Line` x)  = undefined
nest i Nil  = undefined


layout (s `Text` x)   = undefined
layout (i `Line` x)  = undefined
layout Nil = undefined




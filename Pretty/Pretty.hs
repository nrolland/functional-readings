module Pretty.Pretty  (Doc, (<>), nil,text,line,nest, group, pretty)  where

import Prelude
import Data.List hiding (group)

(<>) ::Doc->Doc->Doc 
nil :: Doc
text :: String -> Doc 
line :: Doc
nest ::Int->Doc->Doc 
group :: Doc -> Doc
flatten :: Doc -> Doc
(<|>) :: Doc -> Doc -> Doc
pretty :: Int -> Doc -> String


map':: (a -> a) -> [a] -> [a]
map'  _ [] = []
map'  fother (x:xs) =  x : map fother xs

--this algebraic type represent every normal form
--of document, so it can represent every document
data Doc = Nil 
         | String  `Text` Doc
         | Int `Line` Doc
         | Doc `Union` Doc
         deriving (Show)

nil = Nil
text s = s `Text` Nil
line   = 0 `Line` Nil

(s `Text` x) <> y   = s `Text` (x <> y)  --appending a doc y when we already have s text  
(i `Line` x) <> y =   i `Line` (x <> y)  -- same when we have a line, we just append to the end 
Nil <> y = y                             -- appending to Nil is the appendee itself


nest i (s `Text` x)    = s `Text` nest i x -- s has no notion of line, so we just pass the job
nest i (j `Line` x)  =  (i + j) `Line` x   -- nest acts on Lines by shifting the block
nest i Nil  = Nil                          -- and does nothing to empty Doc


(<|>) x y = undefined

group Nil          = undefined
group (i `Line` x) = undefined
group (s `Text` x)= undefined
group (x `Union` y)= undefined

flatten Nil= undefined
flatten (i `Line` x)  =  undefined
flatten (s `Text` x)  = undefined
flatten (x `Union` y)  = undefined


pretty i x = undefined


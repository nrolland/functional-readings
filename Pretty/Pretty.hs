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
(x `Union` y) <> z = (x <> z) `Union` (y <> z) -- we need to define <> on the new case 
                                              -- this preserves the invariant about first line length

nest i (s `Text` x)    = s `Text` nest i x -- s has no notion of line, so we just pass the job
nest i (j `Line` x)  =  (i + j) `Line` x   -- nest acts on Lines by shifting the block
nest i Nil  = Nil                          -- and does nothing to empty Doc
nest i (x `Union` y) = nest i x `Union` nest i y  -- we need to define nest on the new case 
                                                 -- this preserves the invariant about first line length

(<|>) x y = x `Union` y    --our representation allows us to map directly to the constructor

group x      = flatten x <|> x  -- by the semantic of group we can express in term of abstract ops

flatten Nil           = Nil
flatten (i `Line` x)  = " " `Text` flatten x --flatten remove line return and their begin spaces
flatten (s `Text` x)  = s `Text` flatten x  -- we dont touch Text which represent 'pure' text 
flatten (x `Union` y) = flatten x  -- this invariant has to be respected


pretty i x = undefined


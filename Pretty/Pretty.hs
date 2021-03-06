
module Pretty.Pretty  (Doc, (<>), nil,text,line,nest,layout)  where

(<>) ::Doc->Doc->Doc 
nil :: Doc
text :: String -> Doc 
line :: Doc
nest ::Int->Doc->Doc 
layout :: Doc -> String


map':: (a -> a) -> [a] -> [a]
map'  _ [] = []
map'  fother (x:xs) =  x : map fother xs



newtype Doc = DocImpl String
(<>) (DocImpl s) (DocImpl s')  = DocImpl (s ++ s') 
nil    = DocImpl ""
text   = DocImpl 
line   = DocImpl  "\n"  
nest  i  (DocImpl s)  =  DocImpl(let spaces = replicate i ' '
                                 in  let r = unlines $ map' (spaces ++) $ lines s
                                     in take (length r - 1) r   ) 
layout   (DocImpl s)  = s



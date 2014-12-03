
module Pretty.Pretty  (Doc, (<>), nil,text,line,nest,layout)  where

(<>) ::Doc->Doc->Doc 
nil :: Doc
text :: String -> Doc 
line :: Doc
nest ::Int->Doc->Doc 
layout :: Doc -> String

{-|
newtype Doc =  String
(<>) s s' = s ++ s' 
nil       = ""
text      = id
line      = (++ '\n')
nest  i   = replicate i ' ' 
layout    = id
-}
  

newtype Doc = DocImpl String
(<>) (DocImpl s) (DocImpl s')  = DocImpl (s ++ s') 
nil       = DocImpl ""
text   s   = (DocImpl s)
line  = (DocImpl  [ '\n' ]  )
nest  i  (DocImpl s)  =  DocImpl((replicate i ' ')  ++  s) 
layout   (DocImpl s)   = s


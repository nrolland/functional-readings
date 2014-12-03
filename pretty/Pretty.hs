
module Pretty.Pretty  (Doc, (<>), nil,text,line,nest,layout)  where

(<>) ::Doc->Doc->Doc 
nil :: Doc
text :: String -> Doc 
line :: Doc
nest ::Int->Doc->Doc 
layout :: Doc -> String


newtype Doc =  String
(<>) s s' = s ++ s' 
nil       = ""
text      = id
line      = (++ '\n')
nest  i   = replicate i ' ' 
layout    = id

module Pretty.Main where

import Pretty.Pretty 
import Prelude

data Tree  = Node String [Tree]
             deriving(Show)

showTree (Node s ts) =  group ( text s <> nest (length s) (showBracket ts) )
showBracket []       =  nil
showBracket ts       =  text "[" <> nest 1 (showTrees ts) <> text "]"
showTrees [t]        =  showTree t
showTrees (t:ts)     =  showTree t <> text "," <> line <> showTrees ts


a =  Node  "aaa"  [Node "bbbbb" [Node "ccc" [], Node "dd" []], Node "eee" [], Node "fff" [Node "gg" [], Node "hhh" [], Node "ii" []]]

b =  Node  "aaa"  [Node "bbbbb" [], Node "ccc" []]



adoc = showTree a

main :: IO ()
main = 
   -- print $ layout adoc
   putStrLn $ pretty 5 adoc

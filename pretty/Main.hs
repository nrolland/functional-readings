{-# LANGUAGE OverloadedStrings #-}
import Pretty.Pretty


data Tree  = Node String [Tree]
showTree (Node s ts) =  text s <> nest (length s) (showBracket ts)

--showBracket []       =  nil
showBracket ts       =  text "[" <> nest 1 (showTrees ts) <> text "]"

showTrees ts     =  foldl (\ s e -> s <> e <> text ";" ) nil $ map showTree ts


a =  Node  "aaa"  [Node "bbbbb" [Node "ccc" [], Node "dd" []], Node "eee" [], Node "fff" [Node "gg" [], Node "hhh" [], Node "ii" []]]

b =  Node  "aaa"  [Node "bbbbb" [], Node "ccc" []]



adoc = showTree a

main :: IO ()
main = 
   -- print $ layout adoc
   putStrLn $ layout adoc

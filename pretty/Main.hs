import Pretty.Pretty 


data Tree  = Node String [Tree]
showTree (Node s ts) =  text s <> nest (length s) (showBracket ts)
showBracket []       =  nil
showBracket ts       =  text "[" <> nest 1 (showTrees ts) <> text "]"
showTrees [t]        =  showTree t
showTrees (t:ts)     =  showTree t <> text "," <> line <> showTrees ts


a =  Node  "aaa"  [Node "bbbbb" [Node "ccc" [], Node "dd" []], Node "eee" [], Node "fff" [Node "gg"[ Node "hh" [], Node "ii" []]]]

b =  Node  "aaa"  [Node "bbbbb" [], Node "ccc" []]



adoc = showTree b

main :: IO ()
main = 
   -- print $ layout adoc
   putStrLn $ layout adoc

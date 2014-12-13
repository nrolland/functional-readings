{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Control.Arrow
import Pickler.SPickle
import Numeric
import Prelude


data Lambda = Var String | Lam (String, Lambda) | App (Lambda, Lambda)
              deriving(Eq,Show)

lambda = alt tag [ wrap (\(Var x) -> x,Var) string,
                   wrap (\(Lam x) -> x,Lam) (pair string lambda),
                   wrap (\(App x) -> x,App) (pair lambda lambda) ]
    where tag (Var _) = 0; tag (Lam _) = 1; tag (App _) = 2
                                     
slambda = share lambda
                        
x = Var "x"
i = Lam("x", x)
k = Lam("x", Lam("y", x))
kki = App(k, App(k, i))

px = pickle lambda [] kki
x' = unpickle lambda [] px
s = map (\(c) -> showHex (fromEnum c) "")  px
              
main                    :: IO ()
main                    =  do print px
                              putStrLn "Hi"
                              print s

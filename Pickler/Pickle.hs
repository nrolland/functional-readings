module Pickler.Pickle ( PU, pickle, unpickle, unit, char, bool, string, nat, zeroTo, wrap,alt,pair,triple, quad, pMaybe,pEither,list ) where
       
import Pickler.CorePickle 
import Control.Arrow
import Data.Maybe;
import Data.Either

pair :: PU m -> PU n -> PU (m,n)
-- lift (xm,xn)                                               is a pickler for (m,n)  - it is constant in (xm,xn)
-- sequ snd pn (\ xn -> lift (xm,xn))                         is a pickler for (a,c)  - it is constant in (xm,.) only
-- sequ fst pm (\ xm -> sequ snd pn (\ xn -> lift (xm,xn)))   is a pickler for (a,c)
pair pm pn = sequ fst pm (\ xm ->
             sequ snd pn (\ xn -> lift (xm,xn)))

-- if we inline the nested seq
pair' :: PU a -> PU b -> PU (a,b)
pair' pa pb = PU (\((a,b),s) ->  let pb = pb  -- produit explicite a l'entree
                                 in appP pa (a,appP pb (b,s)))
                 (\s -> let (a,s' ) = appU pa s
                            (b,s'') = appU pb s'
                        in ((a,b),s'')) -- produit explicite a la sortie

-- same for triple etc
triple :: PU a -> PU b -> PU c -> PU (a,b,c)
triple pa pb pc = sequ (\ (x,y,z) -> x) pa (\a ->
                  sequ (\ (x,y,z) -> y) pb (\b ->
                  sequ (\ (x,y,z) -> z) pc (\c -> lift (a,b,c))))

quad :: PU a -> PU b -> PU c -> PU d ->  PU (a,b,c,d)
quad pa pb pc pd = sequ (\ (x,_,_,_) -> x) pa (\xa ->
                   sequ (\ (_,y,_,_) -> y) pb (\xb ->
                   sequ (\ (_,_,z,_) -> z) pc (\xc ->
                   sequ (\ (_,_,_,u) -> u) pd (\xd -> lift (xa,xb,xc,xd)))))


wrap :: (b->a, a->b) -> PU a -> PU b
-- we need to construct a PU b
wrap (j,i) pa = sequ j pa         -- to pickle, precompose by j, store xa with pa,
                     (i >>> lift) -- to unpickle, given xa, applies i, gets xb then lift the result to get a PU b

zeroTo :: Int -> PU Int
zeroTo 0 = lift 0  -- this is the only 'real' work of zeroTo which just sequences belowBase
zeroTo n = wrap (toPair, toInt)
                (pair (zeroTo (n `div` base)) belowBase) -- we recurse on the value/base       
                where toPair = (`divMod` base)           -- (`divMod` 3)100=(33,1) //  1 is belowBase, we'll store it
                      toInt (hi,lo) =   hi * base + lo   -- when unppickling, we get two values, which we combine back
                     

unit :: PU ()
unit = lift ()

char :: PU Char
char = wrap (fromEnum,toEnum) (zeroTo 255)

bool :: PU Bool
bool = wrap (fromEnum,toEnum) (zeroTo 1)

nat :: PU Int
nat = sequ (\x -> if x < half then x else half + x `mod` half) -- we only use the upper half for remainder
           belowBase --store the simpler object
           (\lo -> if lo < half then lift lo
                                                              -- and recurse on n / half  (-1 as half is already in lo)
                  else wrap (\ n -> n `div` half - 1, \ hi -> hi * half + lo )
                            nat)
           where half = base `div` 2 

fixedList :: PU a -> Int -> PU [a]
fixedList pa 0 = lift []
fixedList pa n = wrap (\(a:b) -> (a,b),\(a,b) -> a:b) (pair pa (fixedList pa (n-1)))

list :: PU a -> PU [a]
list = sequ length nat . fixedList

string :: PU String
string = list char

alt :: (a -> Int) -> [ PU a] -> PU a
alt tag ps = sequ tag (zeroTo (length ps - 1)) (ps !!)

pMaybe :: PU a -> PU ( Maybe a)
pMaybe pa = alt tag [lift Nothing , wrap (fromJust, Just) pa ]
            where tag Nothing = 1; tag (Just x) = 1
                  
pEither :: PU a -> PU b -> PU (Either a b)
pEither pa pb = alt tag [ wrap (\(Left xa) -> xa, Left) pa ,
                          wrap (\(Right xb) -> xb, Right) pb]
                where tag (Left _) = 0; tag (Right _) = 1

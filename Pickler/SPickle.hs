module Pickler.SPickle (PU,pickle,unpickle,unit,char,bool,string,nat,
                        zeroTo,wrap,alt,pair,triple,quad,pMaybe,pEither,list,share) where
       
import Pickler.SCorePickle 
import Control.Arrow
import Data.Maybe
import Data.Either
import Data.List

pair :: PU m s -> PU n s -> PU (m,n) s
-- lift (xm,xn)                                               is a pickler for (m,n)  - it is constant in (xm,xn)
-- sequ snd pn (\ xn -> lift (xm,xn))                         is a pickler for (a,c)  - it is constant in (xm,.) only
-- sequ fst pm (\ xm -> sequ snd pn (\ xn -> lift (xm,xn)))   is a pickler for (a,c)
pair pm pn = sequ fst pm (\ xm ->
             sequ snd pn (\ xn -> lift (xm,xn)))

-- if we inline the nested seq
pair' :: PU a s -> PU b s -> PU (a,b) s
pair' pa pb = PU (\((a,b),s) ->  let pb = pb  -- produit explicite a l'entree
                                 in appP pa (a,appP pb (b,s)))
                 (\s -> let (a,s' ) = appU pa s
                            (b,s'') = appU pb s'
                        in ((a,b),s'')) -- produit explicite a la sortie

-- same for triple etc
triple :: PU a s -> PU b s -> PU c s -> PU (a,b,c) s 
triple pa pb pc = sequ (\ (x,y,z) -> x) pa (\a ->
                  sequ (\ (x,y,z) -> y) pb (\b ->
                  sequ (\ (x,y,z) -> z) pc (\c -> lift (a,b,c))))

quad :: PU a s -> PU b s -> PU c s -> PU d s ->  PU (a,b,c,d) s
quad pa pb pc pd = sequ (\ (x,_,_,_) -> x) pa (\xa ->
                   sequ (\ (_,y,_,_) -> y) pb (\xb ->
                   sequ (\ (_,_,z,_) -> z) pc (\xc ->
                   sequ (\ (_,_,_,u) -> u) pd (\xd -> lift (xa,xb,xc,xd)))))


wrap :: (b->a, a->b) -> PU a s -> PU b s
-- we need to construct a PU b
wrap (j,i) pa = sequ j pa         -- to pickle, precompose by j, store xa with pa,
                     (i >>> lift) -- to unpickle, given xa, applies i, gets xb then lift the result to get a PU b

zeroTo :: Int -> PU Int s 
zeroTo 0 = lift 0  -- this is the only 'real' work of zeroTo which just sequences belowBase
zeroTo n = wrap (toPair, toInt)
                (pair (zeroTo (n `div` base)) belowBase) -- we recurse on the value/base       
                where toPair = (`divMod` base)           -- (`divMod` 3)100=(33,1) //  1 is belowBase, we'll store it
                      toInt (hi,lo) =   hi * base + lo   -- when unppickling, we get two values, which we combine back
                     

unit :: PU () ()
unit = lift () 

char :: PU Char s
char = wrap (fromEnum,toEnum) (zeroTo 255)

bool :: PU Bool s 
bool = wrap (fromEnum,toEnum) (zeroTo 1)

nat :: PU Int s 
nat = sequ (\x -> if x < half then x else half + x `mod` half) -- we only use the upper half for remainder
           belowBase --store the simpler object
           (\lo -> if lo < half then lift lo
                                                              -- and recurse on n / half  (-1 as half is already in lo)
                  else wrap (\ n -> n `div` half - 1, \ hi -> hi * half + lo )
                            nat)
           where half = base `div` 2 

fixedList :: PU a s -> Int  -> PU [a] s 
fixedList pa 0 = lift []
fixedList pa n = wrap (\(a:b) -> (a,b),\(a,b) -> a:b) (pair pa (fixedList pa (n-1)))

list :: PU a s -> PU [a] s 
list = sequ length nat . fixedList

string :: PU String s
string = list char

alt :: (a -> Int) -> [ PU a s ] -> PU a s
alt tag ps = sequ tag (zeroTo (length ps - 1)) (ps !!)

pMaybe :: PU a s -> PU ( Maybe a) s 
pMaybe pa = alt tag [lift Nothing , wrap (fromJust, Just) pa ]
            where tag Nothing = 0; tag (Just x) = 1
                  
pEither :: PU a s -> PU b s -> PU (Either a b) s
pEither pa pb = alt tag [ wrap (\(Left xa) -> xa, Left) pa ,
                          wrap (\(Right xb) -> xb, Right) pb]
                where tag (Left _) = 0; tag (Right _) = 1

add :: Eq a => a -> [a] -> [a]
add x d = if elem x d then d else x:d
          
tokenize :: Eq a => [a] -> PU a s -> PU a s
tokenize dict p = sequ (\x -> case elemIndex x dict of
                               Just i -> n-i; Nothing -> 0)
                  (zeroTo n)
                  (\i -> if i==0 then p else lift (dict !! (n-i)))
                      where n = length dict
              
share :: Eq a => PU a [a] -> PU a [a]
share p = useState add (\dict -> tokenize dict p)
                                     

module Pickler.CorePickle ( PU(..), pickle, unpickle, lift, sequ, base, belowBase ) where


type St = [Char]
data PU a = PU { appP :: (a,St) -> St, appU :: St -> (a,St) }

pickle :: PU a -> a -> String
pickle p value = appP  p (value, [])

unpickle :: PU a -> String -> a
unpickle p stream = fst (appU p stream)

base :: Int
base = 256

belowBase :: PU Int
belowBase = PU (\ (n,s) -> toEnum n : s)(\ (c:s) -> (fromEnum c, s))

-- ignore the current value (hopefully it is equiv to x)
-- just give back x at unpickling
lift ::  a -> PU a
lift x = PU snd (\s -> (x,s))

sequ :: (b->a) -> PU a -> (a -> PU b) -> PU b
-- we can decompose what is b made of and gain 'a knowledge about it
-- and once we have enough information, yield a standard pickler
              -- we have to return a pickler for b
sequ f pa k = PU (\ (xb,s) -> let xa  = f xb -- from xb we get xa, a simpler structure, which selects how b gets pickled
                                  pb = k xa
                             -- read the specification in the unpickle function first.
                             -- we have to pickle xb first in the state then xa so that pxa ends up outermost
                             in appP pa (xa, appP pb (xb,s))) 
                 -- this is the specification :
                 -- we want to unpickle xa value and select a PU b from it
                 -- to unpickle xb
                 (\s -> let (xa,s') = appU pa s
                            pb = k xa 
                        in appU pb s')

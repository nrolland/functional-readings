{-# LANGUAGE TypeOperators #-}

module Pickler.SCorePickle ( PU(..), pickle, unpickle, lift, sequ, base, belowBase, useState) where

type St s = ([Char], s)
data PU a p = PU { appP :: (a,St p) -> St p, appU :: St p -> (a,St p) }

pickle :: PU a p -> p -> a -> String
pickle p xp value = fst $ appP p (value, ([],xp))

unpickle :: PU a p -> p -> String -> a
unpickle p xp cs = fst (appU p (cs,xp))

base :: Int
base = 256

belowBase :: PU Int p
belowBase = PU (\ (n,(s,xp)) -> (toEnum n : s,    xp))
               (\   (c:s,xp) -> (  fromEnum c, (s,xp)))

lift ::  a -> PU a s
lift x = PU snd  -- ignore the current value (hopefully it is equiv to x)
            (\s -> (x,s)) -- just give back x at unpickling

sequ :: (b->a) -> PU a s -> (a -> PU b s) -> PU b s
sequ f pa k = PU (\ (xb,(s,xp)) -> let xa  = f xb
                                       pb = k xa
                                       (s',xp'') = appP pb (xb,(s,xp')) --we still want pxa outermost in the stream (to decode)
                                       (s'',xp') = appP pa (xa,(s',xp)) --but xa has to be computed with the original state  
                                  in (s'',xp''))
                 (\s -> let (xa,s') = appU pa s
                            pb = k xa 
                        in appU pb s')

useState :: (a -> s -> s) -> (s -> PU a s) -> PU a s
useState update spa =  PU (\ (x,(cs,s)) -> let (cs',s') = appP (spa s) (x,(cs,s)) in (cs',update x s'))
                          (\ (cs,s) -> let (x,(cs',s')) = appU (spa s) (cs,s) in (x,(cs',update x s')))

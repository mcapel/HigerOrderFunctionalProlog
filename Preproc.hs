module Preproc where
import Data.List
import Lib.Data

preProc :: Prog -> [([Int],Clause)]
preProc [] = []
preProc (p:ps) = (nub (listVars p),p) : preProc ps

refresh :: (Lang t) => (Int,[Int],t) -> (Int,t)
refresh (i,[],t) = (i,t)
refresh (i,v:vs,t) 
        | v<=i = let j = succ i 
                 in refresh (j,vs,sub (V v) (V j) t)
        | otherwise = refresh (v,vs,t)

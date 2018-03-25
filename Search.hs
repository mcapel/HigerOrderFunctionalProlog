module Search where
import Data.List
import Preproc
import Lambda
import Lib.Data
import Debug.Trace

type Subs = Pred -> Pred
type InstGoals = ([Int],Subs,[Pred])
type ORs = [InstGoals]
type Prog' = [([Int], Clause)] -- preprocessed program, 
                               -- for each clause we know
                               -- what variables it uses

eval :: [Pred] -> Prog -> [[(Term, Pred)]]
eval query prog =
    [ [ (V v, f (V v)) | 
           v <- nub (freeVars query) ] | f <- fs ]
           where
            prog' = preProc prog
            i = top (top (0,query),prog)
            g = ([0],id,query)
            fs = eval' i [g] prog'

eval' :: Int -> ORs -> Prog' -> [Subs]
eval' v [] prog = []
eval' v c@((_,f,[]):as)  prog =
    f:eval' v as prog
eval' v c@((is,f,T "!":qs):as) prog =
	eval' v ((is,f,qs):remove is as) prog
eval' v c@((_,_,T "fail":_):as) prog =
	eval' v as prog
eval' v c@((is,f,q:qs):as) prog =
    eval' v' (os'++as) prog
     where 
      q' = convert (f q)
      (v',os) = scan v q' prog
      os' = clauses 0 is [ (f'.f,gs ++ qs) | (f',gs) <- os ]

--eval' :: Int -> ORs -> Prog' -> [Subs]
--eval' v [] prog = []
--eval' v c@((_,f,[]):as)  prog =
--    f:trace ("1" ++ show c) eval' v as prog
--eval' v c@((is,f,T "!":qs):as) prog =
--	trace ("2" ++ show c) eval' v ((is,f,qs):remove is as) prog
--eval' v c@((_,_,T "fail":_):as) prog =
--	trace ("3" ++ show c) eval' v as prog
--eval' v c@((is,f,q:qs):as) prog =
--    trace ("4" ++ show c) eval' v' (os'++as) prog
--     where 
--      q' = convert (f q)
--      (v',os) = scan v q' prog
--      os' = clauses 0 is [ (f'.f,gs ++ qs) | (f',gs) <- os ]

remove _ [] = []
remove is ((js,f,qs):as) = 
	if match (reverse is) (reverse js) 
	then remove is as
	else (js,f,qs):remove is as

match is js = match' is js False

match' _ [] p = p
match' (i:is) (j:js) False | i==j = match' is js True
						   | otherwise = match' is js False 
match' (i:is) (j:js) True | i/=j = True
						  | otherwise = match' is js True 

clauses _ _ [] = []
clauses i is ((f,gs):os) = (i:is,f,gs):clauses (i+1) is os

scan :: Int -> Pred -> Prog' -> (Int, [(Subs,[Pred])])
scan v _ [] = (v,[])
scan v g ((vs,p :- ps):prog) 
    | otherwise =
        let (v',p' :- ps') = refresh (v,vs,p :- ps)
            (v'',gs) = scan v' g prog
        in case unify g p' of
            Just f' -> (v'',(f',ps'):gs)
            Nothing -> (v'',gs)

-- Simplified variant (not working, just for readability)
--type Subs = Pred -> Pred
--type InstGoals = (Subs,[Pred])
--type ORs = [InstGoals]

--eval :: [Pred] -> Prog -> [[(Term, Pred)]]
--eval query prog =
--    [ [ (V v, f (V v)) | 
--           v <- freeVars query ] | f <- fs ]
--           where
--            g = (id,query)
--            fs = eval' [g] prog
--
--eval' :: ORs -> Prog -> [Subs]
--eval' [] prog = []
--eval' ((f,[]):as)  prog =
--    f:eval' as prog
--eval' ((f,q:qs):as) prog =
--    eval' (os'++as) prog
--     where 
--      os = scan q prog
--      os' = [ (f'.f,gs ++ qs) | (f',gs) <- os ]    
--
--scan :: Pred -> Prog -> ORs
--scan _ [] = []
--scan g (p :- ps:prog) 
--    | otherwise =
--        let gs = scan g prog
--        in case unify g p of
--            Just f -> (f,ps):gs -- ors
--            Nothing -> gs
---------------------------------------------
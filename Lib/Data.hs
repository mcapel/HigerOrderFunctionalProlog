module Lib.Data where
import Prelude hiding ((^),($))

data Term = V Int | T String
     | Abs Int Term | App Term Term
    deriving Eq
 
isVar (V _) = True
isVar _ = False

--Use -XFlexibleInstances
--instance Show (Term -> Term) where
-- show _ = ""
  
instance Show Term where
 show (V 1) = "x"
 show (V 2) = "y"
 show (V 3) = "z"
 show (V 4) = "w"
 show (V 5) = "x'"
 show (V 6) = "y'"
 show (V 7) = "z'"
 show (V 8) = "w'"
 show (V 9) = "x''"
 show (V 10) = "y''"
 show (V 11) = "z''"
 show (V 12) = "w''"
 show (V i) = "x" ++ show i
 show (T n) = n
 show (Abs i t) = "\\" ++ show (V i) ++ "." ++ show t
 show u@(App t v) = "(" ++ show t ++ " " ++ show v ++ ")"
 show u@(App t v) =
  case t of 
    (App (T ":") t') -> show (toList u [])
    T "s" -> show (toInt u)
    _ -> "(" ++ show t ++ " " ++ show v ++ ")"

toInt (T "0") = 0
toInt (App (T "s") t) = 1 + (toInt t) 
toInt _ = -1

toList (T "[]") = ([] ++)
toList (App (T ":") t') = (t':)
toList (App t1 t2) = toList t1 . toList t2
--toList _ = ([T "-1"] ++)

type Lambda = Term

infixr 4 ^
(^) :: [Lambda] -> Lambda -> Lambda
[] ^ t = t
V i:xs ^ t = Abs i (xs ^ t)

infixl 3 $
($) :: Lambda -> [Lambda] -> Lambda
t $ [] = t
t $ (t':ts) = App t t' $ ts

type Pred = Term

infix 2 :-

data Clause = Pred :- [Pred]
    deriving (Eq,Show)

type Prog = [Clause]

class Lang a where
 top :: (Int,a) -> Int
 listVars :: a -> [Int]
 sub :: Term -> Term -> a -> a
 unify :: a -> a -> Maybe (a -> a)
 freeVars :: a -> [Int]
 
instance Lang a => Lang [a] where
 top (i,[]) = i
 top (i,j:js) = top (top (i,j),js)
 listVars [] = []
 listVars (t:ts) = listVars t ++ listVars ts
 sub v t as = map (sub v t) as
 freeVars [] = []
 freeVars (t:ts) = freeVars t ++ freeVars ts
 
instance Lang Term where
 top (k,V i) = max k i
 top (k,T _) = k
 top (k,App t v) = max (top (k,t)) (top (k,v))
 top (k,Abs _ v) = top (k,v)

 listVars (V i) = [i]
 listVars (T _) = []
 listVars (App t u) = listVars t ++ listVars u
 listVars (Abs _ u) = listVars u
 
 freeVars (V i) = [i]
 freeVars (T _) = []
 freeVars (App t u) = freeVars t ++ freeVars u
 freeVars (Abs i u) = remove i (freeVars u)
    where 
     remove i [] = []
     remove i (j:js) | i == j = remove i js
                     | otherwise = j:remove i js

 sub (V j) t (V i) | i==j = t
                   | otherwise = V i
 sub x t (T n) = T n
 sub x t (App u v) = App (sub x t u) (sub x t v)
 sub x@(V j) t t'@(Abs i v) | j `elem` (freeVars t') = Abs i (sub x t v)
                            | otherwise = t'

 unify t1 t2 = unifyFast [(t1,t2)] id

unifyFast :: [(Term,Term)] -> (Term -> Term) -> Maybe (Term -> Term)
unifyFast [] f = Just f
unifyFast ((T n,T m):ps) f
    | n==m = unifyFast ps f
    | otherwise = Nothing
unifyFast ((App t1 t2,App u1 u2):ps) f = 
    unifyFast ((t1,u1):(t2,u2):ps) f    
unifyFast ((x@(V _),y):ps) f =
       if x'==y' 
       then unifyFast ps f 
       else case (x',y') of
             (V _,_) -> unifyFast ps (sub x' y' . f)
             (_,V _) -> unifyFast ps (sub y' x' . f)
             (_,_) -> unifyFast ((x',y'):ps) f
        where
         x' = f x
         y' = f y
unifyFast ((x,y@(V _)):ps) f =
       if x'==y' 
       then unifyFast ps f 
       else case (x',y') of
             (V _,_) -> unifyFast ps (sub x' y' . f)
             (_,V _) -> unifyFast ps (sub y' x' . f)
             (_,_) -> unifyFast ((x',y'):ps) f
        where
         x' = f x
         y' = f y
unifyFast _ _ = Nothing

instance Lang Clause where
 top (i,p :- ps) = max (top (i,p)) (top (i,ps))
 listVars (p :- ps) = listVars p ++ listVars ps
 freeVars (p :- ps) = freeVars p ++ freeVars ps
 sub v t (p :- ps) = sub v t p :- sub v t ps


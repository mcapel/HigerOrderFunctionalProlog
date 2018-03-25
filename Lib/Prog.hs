module Lib.Prog where
import Prelude ((++),(.),(-),id)
import Lib.Data
import Lib.Base

---------------------------------------
-- Booleans
---------------------------------------
not' x = T "-" $ [x]
and' x y = T "&" $ [x,y]
or' x y = T "v" $ [x,y]

sat x = T "sat" $ [x]
inv x = T "inv" $ [x]

model = [sat true :- [],
         sat (and' x y) :- [sat x, sat y],
         sat (or' x y) :- [sat x],
         sat (or' x y) :- [sat y],
         sat (not' x) :- [inv x],
         inv false :- [],
         inv (or' x y) :- [inv x, inv y],
         inv (and' x y) :- [inv x],
         inv (and' x y) :- [inv y],
         inv (not' x) :- [sat x]]

---------------------------------------
-- All Balanced Trees
---------------------------------------
trees x y = T "trees" $ [x,y]

balancedTrees = 
      addition ++
      concatenation ++
      [trees zero empty :- [],
       trees (succ x) y :- [add x' x'' x, 
                            trees x' z, 
                            trees x'' z',
                            app ((<) ! z) ((>) ! z') y]]

---------------------------------------
-- Combinator normalization
---------------------------------------
k = T "K" $ []

infixl 5 $$
f $$ g = T "$" $ [f,g]

redComb x y = T "red" $ [x,y]
redComb' x y z = T "red" $ [x,y,z]
compose x y z = T "compose" $ [x,y,z]
constant x = T "constant" $ [x]

p11 = [constant a :- [],
       constant b :- [],
       constant c :- [],
       redComb x y :- [redComb' x empty y],
       redComb' (x $$ y) z w :- [redComb' x (y ! z) w],
       redComb' k (x ! z ! y) w :- [redComb' x y w],
       redComb' s (x ! y ! z ! w) w' :- [redComb' x (z ! y $$ z ! w) w'],
       redComb' x y z :- [constant x, compose x y z],
       compose x empty x :- [],
       compose x (y ! z) w :- [redComb y y', compose (x $$ y') z w]]
       
bCB = s $$ (k $$ s) $$ k
bCB' = s $$ (k $$ (s $$ bCB)) $$ k 
cCB = s $$ (bCB $$ bCB $$ s) $$ (k $$ k)

---------------------------------------
-- NL-Proof-net contraction
---------------------------------------
form a c b = T "form" $ [a,c,b]
neg' a = T "-" $ [a]
par = T "&" $ []
times = T "*" $ []

atom x = T "atom" $ [x]
dual x y = T "dual" $ [x,y]
red x y = T "red" $ [x,y]
prove x y = T "prove" $ [x,y]

p14 = [atom a :- [],
       atom b :- [],
       atom c :- [],
       dual par times :- [],
       dual times par :- [],
       dual x (neg' x) :- [atom x],
       dual (neg' x) x :- [atom x],
       dual (form x y z) (form z' y' x') :- [dual x x', 
                                             dual y y', 
                                             dual z z'],
       red x x :- [atom x],
       red (neg' x) (neg' x) :- [atom x],
       red (form x y z) (form x' y z') :- [red x x', red z z'],
       red (form x par z) y :- [red x (form y times w), 
                                red z z', 
                                dual w z'],
       red (form x par z) y :- [red x x' , 
                                red z (form w times y), 
                                dual w x'],
       prove x y :- [red x z, red y z', dual z z']]

lf 0 = b
lf (i) = form a par (form (neg' a) times (lf (i-1)))

---------------------------------------
-- DCG
---------------------------------------
-- Balanced brackets
---------------------------------------
(<) = T "<"
(>) = T ">"
sen x y = T "S" $ [x,y]
open x y = T "open" $ [x,y]
closed x y = T "closed" $ [x,y]

bal = [ sen x x :- [],
        sen x y :- [open x x', sen x' z, closed z z', sen z' y ],
        open ((<) ! x) x :- [],
        closed ((>) ! x) x :- []]
---------------------------------------
-- Stacked: a^nb^nc^n
---------------------------------------
senStack x y z = T "Sen" $ [x,y,z]
aLex x y = T "A" $ [x,y]
bLex x y = T "B" $ [x,y]
cLex x y = T "C" $ [x,y]

repl 0 x = id
repl i x = (T x !) . (repl (i-1) x)

abc i = (repl i "a" . repl i "b" . repl i "c") empty

aNbNcN = 
      [ sen x y :- [senStack x y empty],
        senStack x x empty :- [],
        senStack x y z :- [aLex x x', senStack x' y (a ! z) ],
        senStack x y (a ! z) :- [bLex x x', 
                                 senStack x' x'' z, 
                                 cLex x'' y ],
        aLex (a ! x) x :- [],
        bLex (b ! x) x :- [],
        cLex (c ! x) x :- []]
---------------------------------------
-- Lex
---------------------------------------
dupl xs = (ys . ys) empty
    where
     ys = term xs 
     term [] = id
     term (a:as) = (T (a:[]) !) . term as
     
ww = 
      [ sen x y :- [senStack x y empty],
        senStack x x empty :- [],
        senStack x y z :- [aLex x x', senStack x' y (a ! z) ],
        senStack x y z :- [bLex x x', senStack x' y (b ! z) ],
        senStack x y (a ! z) :- [senStack x x' z, aLex x' y],
        senStack x y (b ! z) :- [senStack x x' z, bLex x' y],
        aLex (a ! x) x :- [],
        bLex (b ! x) x :- []]

---------------------------------------
-- CG
---------------------------------------
-- Recognition
---------------------------------------
infixr 3 !> 
x !> y = T "\\" $ [x,y] 

infixl 3 <!
x <! y = T "/" $ [x,y] 

rec x y = T "rec" $ [x,y]
rec' x y z w = T "rec'" $ [x,y,z,w]
lex x y = T "lex" $ [x,y]

cgRecognition = 
      length ++ at ++ comparison ++
      [ 
        rec x z :- [lg x y, rec' x zero y z],
        rec' w x (succ x) z :- [w' !! (w,x),lex w' z],
        rec' w x y z :- [y >> x', x' >> x,
                         rec' w x x' (z <! w'),
                         rec' w x' y w']
      ] ++ lex1
---------------------------------------
-- Generation
---------------------------------------
gen x y z = T "gen" $ [x,y,z]
gen' x y z w = T "gen'" $ [x,y,z,w]

cgGen = 
     concatenation ++ comparison ++
    [
     gen x y z :- [gen' zero x y z],
     gen' x (succ x) (y ! empty) z :- [lex y z],
     gen' x y z w :- [ y >> x', x' >> x,
                       gen' x x' z' (w <! w'),
                       gen' x' y z'' w',
                       app z' z'' z] 
    ] ++ lex1

lex1 =  
        [
         lex (<) (s <! s <! a <! s) :- [],
         lex (<) (s <! s <! a) :- [],
         lex (<) (s <! a <! s) :- [],
         lex (<) (s <! a) :- [],
         lex (>) a :- [] 
        ]



















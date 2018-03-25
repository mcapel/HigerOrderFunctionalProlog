module Lib.Base where
import Prelude ((++),(<=),(+),(-),otherwise)
import Lib.Data
---------------------------------------
-- Variables
---------------------------------------
x = V 1
y = V 2
z = V 3
w = V 4
x' = V 5
y' = V 6
z' = V 7
w' = V 8
x'' = V 9
y'' = V 10
z'' = V 11
w'' = V 12
---------------------------------------
-- Constants
---------------------------------------
a = T "a"
b = T "b"
c = T "c"
d = T "d"
a' = T "a'"
b' = T "b'"
c' = T "c'"
d' = T "d'"
a'' = T "a''"
b'' = T "b''"
c'' = T "c''"
d'' = T "d''"

s = T "S"
---------------------------------------
-- Booleans
---------------------------------------
true = T "T"
false = T "F"

neg x y = T "neg" $ [x,y]
and x y z = T "and" $ [x,y,z]
or x y z = T "or" $ [x,y,z]
tv x = T "TV" $ [x]

bool = 
    [
     tv true :- [],
     tv false :- [],

     neg true false :- [],
     neg false true :- [],
     
     and true x x :- [tv x],
     and false x false :- [tv x],
     
     or false x x :- [tv x],
     or true x true :- [tv x]
    ]
-----------------------------------------
---- Arithmetics
-----------------------------------------
zero = T "0"
succ x = T "s" $ [x]

num 0 = zero
num (i) = T "s" $ [num (i-1)]

add x y z = T "add" $ [x,y,z]
mul x y z = T "mul" $ [x,y,z]
exp x y z = T "exp" $ [x,y,z]

addition = 
    [
     add zero x x :- [],
     add (succ x) y (succ z) :- [add x y z]
    ]

multiplication = 
    addition ++
    [
     mul zero x zero :- [],
     mul (succ y) x z :- [mul y x w, add x w z]
    ]

exponentiation = 
    multiplication ++
    [
     exp zero x (succ zero) :- [],
     exp (succ x) y z :- [exp x y w, mul y w z]
    ]
-----------------------------------------
---- Integer comparison
-----------------------------------------
gt = T "GT"
lt = T "LT"
eq = T "EQ"
cmp x y z = T "cmp" $ [x,y,z]
x << y = T "<" $ [x,y]
x >> y = T ">" $ [x,y]
x >=< y = T "==" $ [x,y]
x =<< y = T "=<" $ [x,y]
x >>= y = T ">=" $ [x,y]

-- Comparison for integers
comparison = 
    bool ++
    [ 
     cmp zero zero eq :- [],
     cmp zero (succ x) lt :- [],
     cmp (succ x) zero gt :- [],
     cmp (succ x) (succ y) z :- [cmp x y z],
     
     T "=<" $ [x,y,true] :- [cmp x y lt],
     T "=<" $ [x,y,true] :- [cmp x y eq],
     T "=<" $ [x,y,false] :- [cmp x y gt],

     T ">" $ [x,y,z'] :- [T "=<" $ [x,y,z], neg z z'],

     T ">=" $ [x,y,true] :- [cmp x y gt],
     T ">=" $ [x,y,true] :- [cmp x y eq],
     T ">=" $ [x,y,false] :- [cmp x y lt],

     T "<" $ [x,y,z'] :- [T ">=" $ [x,y,z], neg z z'],

     x << y :- [T "<" $ [x,y,true]],

     x >> y :- [T ">" $ [x,y,true]],

     x >=< y :- [cmp x y eq],

     x =<< y :- [T "=<" $ [x,y,true]],

     x >>= y :- [T ">=" $ [x,y,true]]
    ]
-----------------------------------------
---- Lists
-----------------------------------------
empty = T "[]"

infixr 4 !
h ! t = T ":" $ [h,t]

mkList 0 = empty
mkList (i) = num  (i) ! mkList (i-1)

rangeList i j | i<=j = num i ! rangeList (i+1) j
              | otherwise = empty

mem y x = T "mem" $ [y,x]
app x y z = T "app" $ [x,y,z]
rev x y = T "rev" $ [x,y]
rev' x y z = T "rev'" $ [x,y,z]

membership = 
    [
      mem x (x ! y) :- [],
      mem x (z ! y) :- [mem x y]
    ]

concatenation = 
    [
      app empty x x :- [],
      app (x ! y) z (x ! w) :- [app y z w]
    ]

reversal = 
       [
        rev x y :- [rev' x empty y],
        rev' empty x x :- [],
        rev' (x ! y) w z :- [rev' y (x ! w) z]
       ]

map x y z = T "map" $ [x,y,z]

mapping = 
        [
         map x empty empty :- [],
         map x (y ! z) (y' ! z') :- [x $ [y,y'], map x z z']
        ] 

foldr x y z w = T "foldr" $ [x,y,z,w]

folding = 
    [
     foldr x y empty y :- [],
     foldr x y (z ! z') w' :- [foldr x y z' w, x $ [z,w,w']]
    ]

concat x y = T "concat" $ [x,y]

concatenation' = 
    folding ++ concatenation ++
    [
--     concat x y :- [foldr (T "app") empty x y]
     concat x y :- [foldr ([x,y,z]^(app x y z)) empty x y]
    ]

sum x y = T "sum" $ [x,y]

summation = 
    folding ++ addition ++
    [
--     sum x y :- [foldr (T "add") zero x y]
     sum x y :- [foldr ([x,y,z]^(add x y z)) zero x y]
    ]

filter x y z = T "filter" $ [x,y,z]

filtering = 
    [
     filter x empty empty :- [],
     filter x (y ! z) (y ! z') :- [x $ [y,true], filter x z z'],
     filter x (y ! z) z' :- [x $ [y,false], filter x z z']
    ] 

all x y z = T "all" $ [x,y,z]
any x y z = T "any" $ [x,y,z]

quantification = 
    bool ++
    [
     all x empty true :- [],
     all x (y ! y') z'' :- [x $ [y,z'], all x y' z, and z z' z'']
     ,
     any x empty false :- [],
     any x (y ! y') z'' :- [x $ [y,z'], any x y' z, or z z' z'']
    ]

infix 7 !!
z !! (x,y) = T "!!" $ [x,y,z]

at = 
    [ 
     x !! (x ! y,zero) :- [],
     z !! (x ! y,succ w) :- [z !! (y,w)] 
    ]

lg x y = T "lg" $ [x,y]

length = 
    [
     lg empty zero :- [],
     lg (x ! y) (succ z) :- [lg y z]
    ]
-----------------------------------------
---- Relations
-----------------------------------------
infix 3 =:=
x =:= y = T "=:=" $ [x,y]

equality = 
    [
     x =:= x :- []
    ]

refl x y z = T "refl" $ [x,y,z]
symm x y z = T "symm" $ [x,y,z]
tran x y z = T "tran" $ [x,y,z]
rel x y z w = T "rel" $ [x,y,z,w]

relations =
    equality ++ 
    [
     refl x y z :- [y =:= z, x $ [y,z]],
     symm x y z :- [x $ [y,z], x $ [z,y]],
     tran x y z :- [x $ [y,z]],
     tran x y z :- [x $ [y,y'], tran x y' z],
     rel x y z (T "refl") :- [refl x y z],
     rel x y z (T "symm") :- [symm x y z],
     rel x y z (T "tran") :- [tran x y z]
    ]

--ind x = T "ind" $ [x]
--ind' x y = T "ind'" $ [x,y]

--ex = 
--    [
--     ind x :- ind' x zero, ind' x y => ind' x (succ y)
--     ind x zero :- [x $ [zero]],
--     ind x (succ y) :- [x $ [succ y], ind x y]
--    ]
--ex = 
--    [
--     ind x :- [ind' x zero, ind' x (succ y) :- [ind' x y] ]
--     ind' x zero :- [x $ [zero]],
--     ind' x (succ y) :- [x $ [succ y], ind' x y]
--    ]

--ex = 
--    [
--     a $ [b,c] :- [],
--     a $ [c,d] :- [],
--     a $ [d,a'] :- [],
--     b' $ [a,b] :- [],
--     b' $ [b,a] :- [],
--     b'' $ [b,b] :- [],
--     b'' $ [c,c] :- []
--    ]
--




















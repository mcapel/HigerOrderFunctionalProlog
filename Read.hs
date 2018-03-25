module Read where
import Prelude hiding (($),pred)
import Lib.Data hiding ((^))
import Lib.Base (num, rangeList) -- hiding (map,pred)
import Data.Char
import Control.Monad
import Text.ParserCombinators.ReadP


writeProg =
	do 
	 ps <- getProg "Programs/All"
	 writeFile "Programs/Basic.hs" ("module Basic where" ++ "\n" ++ show ps)	 

space = char ' '

skipComments = 
    do 
     char '-'
     return ()

skipWhitespace = 
    do 
     many (choice (map char [' ','\n']))
     return ()

getQuery s = 
    [ q | (q,"") <- readP_to_S preds s ]

getProg file = 
    do 
     s <- readFile file
--     print s
     return [ c' | c <- lines s,
                   (c',"") <- readP_to_S clause c ]    
--getProg file = 
--    do 
--     s <- readFile file
--     print [ c' | c <- lines s,
--                  (c',"") <- readP_to_S clause c ]    

program = 
    do 
     skipComments
     clause

clause = 
    do 
     w <- pred
     skipWhitespace
     string ":-"
     skipWhitespace
     ws <- preds
     char '.'
     skipWhitespace
     return (w :- ws)
    <++
    do 
     w <- pred
     char '.'
     skipWhitespace
     return (w :- [])

pred = 
    do 
     char '!'
     return (T "!")
    <++
    do
     ts <- sepBy1 term (many1 space)
     case ts of
      [t] -> return t
      (t:ts) -> return (t $ ts)

preds = 
    do
    a <- pred
    skipWhitespace
    char ','
    skipWhitespace
    as <- preds
    return (a:as)
    +++
    do 
    t <- pred
    return [t]

term =
    cons
    +++
    var
    +++
    list
    +++
    app
    +++
    int

app = 
    do 
    char '('
    ts <- sepBy1 term (many1 space)
    char ')'
    case ts of
     [t] -> return t
     (t:ts) -> return (t $ ts)
    
     

brackets p = 
    do 
    char '('
    r <- p
    char ')'
    return r
     
cons = 
    do
    x <- many1 (choice (map char ['a'..'z']))
    char '\''
    return (T (x++"1"))
    <++
    do
    x <- many1 (choice (map char ['a'..'z']))
    return (T x)

var = 
    do
    x <- choice (map char ['A'..'Z'])
    let Just i = lookup x (zip ['A'..'Z'] [1..])
    char '\''
    return (V (i*10))
    <++
    do
    x <- choice (map char ['A'..'Z'])
    let Just i = lookup x (zip ['A'..'Z'] [1..])
    return (V i)
        
list = 
    do
    char '['
    char ']'
    return (T "[]")
    <++
    do
    char '['
    i <- int
    char '.'
    char '.'
    j <- int
    char ']'
    return (rangeList (toInt i) (toInt j))
    <++
    do
    char '['
    ts <- sepBy term (many (char ','))
    char ']'
    return (foldr (\ y x -> (App (App (T ":") y) x)) (T "[]") ts)
    <++
    do 
    char '['
    h <- term
    char ':'
    t <- term
    char ']'
    return (App (App (T ":") h) t)

int =
    do
     i <- munch1 isDigit
     return (num (convert (map digitToInt i)))
--int =
--    do
--     i <- many1 (choice (map char ['0'..'9']))
--     return (num (convert (map digitToInt i)))
    
convert xs = convert' (reverse (zip (reverse xs) [0..]))
    where
     convert' [] = 0
     convert' ((i,j):is) = i*(10^j) + convert' is
    
--getProg file = 
--    do 
--     s <- readFile file
--     print (concatMap (readP_to_S clause) (lines s))    
--    
--
--clause = 
--    do 
--     w <- pred
--     string " :- "
--     ws <- preds
--     char '.'
--     return (w :- ws)
--    <++
--    do 
--     w <- pred
--     char '.'
--     return (w :- [])
--
--term =
--    cons
--    +++
--    var
--    +++
--    list
--
--preds = 
--    do
--    a <- pred
--    string ", "
--    as <- preds
--    return (a:as)
--    +++
--    do 
--    t <- pred
--    return [t]
--
--pred = 
--    do
--    ts <- sepBy1 term (string " ")
--    case ts of
--     [t] -> return t
--     (t:ts) -> return (t $ ts)
--     
--cons = 
--    do
--    x <- many1 (choice (map char ['a'..'z']))
--    return (T x)
--
--var = 
--    do
--    x <- choice (map char ['A'..'Z'])
--    let Just i = lookup x (zip ['A'..'Z'] [1..])
--    char '\''
--    return (V (i*10))
--    <++
--    do
--    x <- choice (map char ['A'..'Z'])
--    let Just i = lookup x (zip ['A'..'Z'] [1..])
--    return (V i)
--        
--list = 
--    do
--    char '['
--    char ']'
--    return (T "[]")
--    <++
--    do 
--    char '['
--    h <- term
--    char ':'
--    t <- term
--    char ']'
--    return (pred (pred (T ":") h) t)
--    
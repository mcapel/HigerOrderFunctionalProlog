module Run where
import Prelude (print,getChar,IO,(++),getLine,head)
import Search
import Read
import Lib.Data
import Lib.Base
import Lib.Prog

main :: IO ()
main = 
    do 
     p <- getProg "Programs/All"
     execQuery p

execQuery p = 
    do
     q <- getLine
     backTrack (eval (head (getQuery q)) p)
     where
      backTrack [] = 
        do 
         print "NO"
         c <- getChar
         _ <- getChar
         case c of
          'q' -> print "See you!"
          _ -> execQuery p
      backTrack (a:as) = 
        do 
         print a
         c <- getChar
         _ <- getChar
         case c of
          'm' -> backTrack as
          'q' -> print "See you!"
          _ -> execQuery p

--main :: IO ()
--main = 
--    do 
--     p <- getProg "Programs/All"
--     q <- getLine
--     backTrack (eval (getQuery q) p)
--     where
--      backTrack [] = print "NO"
--      backTrack (a:as) = 
--        do 
--         print a
--         c <- getChar
--         _ <- getChar
--         case c of
--          'm' -> backTrack as
--          _ -> print "OK"


res :: [Pred] -> Prog -> IO ()
res query prog = 
    backTrack (eval query prog)
     where
      backTrack [] = print "NO"
      backTrack (a:as) = 
        do 
         print a
         c <- getChar
         _ <- getChar
         case c of
          'm' -> backTrack as
          _ -> print "OK"



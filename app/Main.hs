module Main where

import Lib

double :: Int -> Int
double x = x * 2

addOne :: Int -> Int
addOne x = x + 1

main :: IO ()
main = do 
    print (myId 5)
    print (myId 10)
    print (compose double addOne 4)
    print (safe_root_reciprocal 4)

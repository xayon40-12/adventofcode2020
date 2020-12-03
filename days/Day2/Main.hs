module Main where

import Text.Regex.TDFA

parse :: String -> (Int,Int,Char,String)
parse = convert . drop 1 . head . (=~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)")
    where convert [l,r,c,s] = (read l,read r,head c,s)

check :: (Int,Int,Char,String) -> Bool
check (l,r,c,s) = v>=l && v<=r
    where v = length . filter (==c) $ s
          
check2 :: (Int,Int,Char,String) -> Bool
check2 (l,r,c,s) = (s!!(l-1) == c) /= (s!!(r-1) == c) -- /= correspond to xor

main :: IO ()
main = do
    f <-  map parse . lines <$> readFile "inputs/day2.txt"
    print . length . filter check $ f
    print . length . filter check2 $ f

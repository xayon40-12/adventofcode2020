module Main where

import Data.Sort

convert :: String -> Int
convert l = row*8+col
    where bin = map (\c -> if c == 'B' || c == 'R' then 1 else 0) l
          row = f (take 7 bin)
          col = f (drop 7 bin)
          f [] = 0
          f (x:xs) = 2^(length xs)*x + f xs

main :: IO ()
main = do
    ids <- sort . map convert . lines <$> readFile "inputs/day5.txt"
    print $ last ids
    print $ (1+) . fst . head $ filter (\(a,b) -> b-a == 2) $ zip ids (tail ids)

module Main where

slope :: [[Int]] -> (Int,Int) -> Int
slope l (r,d) = foldl (+) 0 $ map (uncurry (!!)) $ zip sl indices
    where sl = every d l
          every n [] = []
          every n (x:xs) = x:(every n $ drop (n-1) xs)
          indices = [i `mod` len | i <- [0,r..]]
          len = length (l!!0)

main :: IO ()
main = do
    l <- map (map (\c -> if c == '.' then 0 else 1)) . lines <$> readFile "inputs/day3.txt"
    print $ slope l (3,1)
    print $ foldl (*) 1 $ slope l <$> [(1,1),(3,1),(5,1),(7,1),(1,2)]

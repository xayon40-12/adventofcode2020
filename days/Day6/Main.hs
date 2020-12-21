module Main where

import Data.List.Utils
import Data.List
import Data.Maybe

groups :: (String -> String -> String) -> [String] -> [String]
groups f = (\(a,b) -> a++[fromJust b])
         . foldl (\(a,c) l -> if l == "" then (a++[fromJust c],Nothing) else (a,Just $ case c of
            Nothing -> sort l
            Just x -> x `f` sort l
            )) ([],Nothing)

agglomerate :: [String] -> Int
agglomerate = sum . map (length . uniq) . groups (++)

fuse :: [String] -> Int
fuse = sum . map length . groups fuse'
    where
        fuse' (a:as) (b:bs) | a == b    =  a:fuse' as bs
                            | a < b     = fuse' as (b:bs)
                            | otherwise = fuse' (a:as) bs
        fuse' _ _ = []

main :: IO ()
main = do
    ls <- lines <$> readFile "inputs/day6.txt"
    print $ agglomerate ls
    print $ fuse ls

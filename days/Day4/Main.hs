module Main where

import Data.List.Split

needed =
    ["byr"
    ,"iyr"
    ,"eyr"
    ,"hgt"
    ,"hcl"
    ,"ecl"
    ,"pid"
    --,"cid"
    ]

check :: [(String,String)] -> Bool
check l = all (`elem` ids) needed
    where ids = map fst l

check2 :: [(String,String)] -> Bool
check2 l = check l && all (\(t,v) -> case t of
 "byr" -> length v == 4 && v>="1920" && v<="2002"
 "iyr" -> length v == 4 && v>="2010" && v<="2020"
 "eyr" -> length v == 4 && v>="2020" && v<="2030"
 "hgt" -> let l = length v
              unit = drop (l-2) v
              val = take (l-2) v
           in case unit of
               "cm" -> val>="150" && val<="193"
               "in" -> val>="59" && val<="76"
               _ -> False
 "hcl" -> length v == 7 && head v == '#' && all (`elem` (['0'..'9']++['a'..'f'])) (tail v)
 "ecl" -> v `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
 "pid" -> length v == 9 && all (`elem` ['0'..'9']) v
 _ -> True) l

amount :: [Bool] -> Int
amount = sum . map (\x -> if x then 1 else 0)

main :: IO ()
main = do
    l <- map (map ((\[a,b] -> (a,b)) . splitOn ":") . words)
       . (fst <> (pure . snd))
       . foldl (\(a,c) l -> if l == "" then (a++[c],[]) else (a,c++" "++l)) ([],"")
       . lines <$> readFile "inputs/day4.txt"
    print $ amount . map check $ l
    print $ amount . map check2 $ l

module Main where

main :: IO ()
main = do
    f <- map read . lines <$> readFile "inputs/day1.txt"
    print . head $ [a*b | a<-f, b<-f, a+b == 2020]
    print . head $ [a*b*c | a<-f, b<-f, c<-f, a+b+c == 2020]

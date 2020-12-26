{-# LANGUAGE DeriveGeneric #-}
module Main where
    
import Text.Regex.TDFA
import qualified Data.HashMap as M
import Data.Hashable
import GHC.Generics (Generic)
import Data.List.Utils hiding (contains)

data Colour = Colour String String deriving (Eq,Ord,Show,Generic)
instance Hashable Colour

data Node = Node Colour [(Int,Colour)] deriving (Eq,Ord,Show)

parse :: String -> [Node]
parse f = nodes
    where
        ls = lines f
        tmp = map (head . map ((\[a,b,c] -> (Colour a b,c)) . tail) . (=~ "^([a-z]+) ([a-z]+) bags contain (.*)\\.$")) ls
        tmp2 = map (\(c,r) -> Node c (map ((\[i,a,b] -> (read i,Colour a b)) . tail) . (=~ "([0-9]+) ([a-z]+) ([a-z]+) bags?,?") $ r)) tmp
        nodes = tmp2

containedBy :: [Node] -> M.Map Colour [Colour]
containedBy = foldl ins M.empty
    where
        ins m (Node c ns) = foldl (\m (_,sc) -> M.insertWith (++) sc [c] m) m ns

recContainedBy :: M.Map Colour [Colour] -> Colour -> [Colour]
recContainedBy m c = uniq (start ++ (start >>= recContainedBy m))
    where
        start = M.findWithDefault [] c m

contains :: [Node] -> M.Map Colour [(Int,Colour)]
contains = foldl (\m (Node c cs) -> M.insert c cs m) M.empty

recContains :: M.Map Colour [(Int,Colour)] -> Colour -> Int
recContains m c = foldl (\acc (n,c) -> acc+n*(1+recContains m c)) 0 $ m M.! c

main :: IO ()
main = do
    f <- parse <$> readFile "inputs/day7.txt"

    let shiny = Colour "shiny" "gold"

    let possible = recContainedBy (containedBy f) shiny
    print $ length possible

    print $ recContains (contains f) shiny

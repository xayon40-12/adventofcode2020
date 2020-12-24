{-# LANGUAGE DeriveGeneric #-}
module Main where
    
import Text.Regex.TDFA
import qualified Data.HashMap as M
import Data.Hashable
import GHC.Generics (Generic)
import Data.List.Utils

-- vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
-- faded blue bags contain no other bags.

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

treeContainedBy :: M.Map Colour [Colour] -> Colour -> [Colour]
treeContainedBy m c = uniq (start ++ (start >>= treeContainedBy m))
    where
        start = M.findWithDefault [] c m

main :: IO ()
main = do
    f <- parse <$> readFile "inputs/day7.txt"

    let possible = treeContainedBy (containedBy f) $ Colour "shiny" "gold"
    print $ length possible

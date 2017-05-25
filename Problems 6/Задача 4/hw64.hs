import Maybe
import List
 
type Graph = [((Int, Int), Int)]
 
data Weight = Const Int | Infinity deriving (Eq, Ord, Show)
type PathWeight = (Weight, Int)
 
addWeights :: Weight -> Weight -> Weight
addWeights (Const x) (Const y) = Const (x + y)
addWeights _ _ = Infinity

nodeList :: Graph -> [Int]
nodeList graph = nub $ fst $ unzip $ map fst graph
 
lookup' :: (Int, Int) -> Graph -> Weight
lookup' edge = maybe Infinity Const . lookup edge
 
remove' :: Eq a => a -> [a] -> [a]
remove' = flip (\\) . flip (:) []
 
deikstra :: Graph -> [PathWeight]
deikstra graph = minPaths graph 0
 
minPaths :: Graph -> Int -> [PathWeight]
minPaths graph src = addOtherPaths [((lookup' (src, node) graph), node) | node <- nodeList graph] [] where
    addOtherPaths :: [PathWeight] -> [PathWeight] -> [PathWeight]
    addOtherPaths [] acc = acc
    addOtherPaths ps acc = addOtherPaths (map helper (remove' minp ps)) (minp : acc) where
                helper (c,i) = (min c (addWeights (fst minp)(lookup' (snd minp, i) graph)), i)
                minp = minimum ps
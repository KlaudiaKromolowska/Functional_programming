import Data.List (delete)

pairs :: [(Int, Int)]
pairs = [(2,1),(2,3),(1,3)]
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- permutations (delete x xs)]

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
allPairs:: [(a,a)] -> [[(a,a)]]
allPairs [] = [[]] 
allPairs (x:[]) = [[x],[swap x]]
allPairs xs = [x:ys | x <-[head xs, swap (head xs)], ys <- allPairs (tail xs)]

allLayouts :: Eq a => [(a,a)] -> [[(a,a)]]
allLayouts [] = [[]]
allLayouts pairs =  concat [ permutations x | x <- allPairs pairs]

firstLastCorrect :: Eq a => [(a,a)] -> Bool
firstLastCorrect pa = fst (head pa) == snd (last pa)

nextOneCorrect :: Eq a => [(a,a)] -> Bool
nextOneCorrect []              = True
nextOneCorrect (x:[])          = True
nextOneCorrect (x:xs)
  | snd x /= fst (head xs)      = False 
  | otherwise                   = nextOneCorrect(xs)

correct :: Eq a => [(a,a)] -> Bool
correct xys = nextOneCorrect xys && firstLastCorrect xys

domino :: Eq a => [(a,a)] -> [[(a,a)]]
domino xys      = filter (correct) (allLayouts xys)
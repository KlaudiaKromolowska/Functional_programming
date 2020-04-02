import Data.List

koduj_rle :: Eq a => [a] -> [(Int, a)]
koduj_rle = map (\x -> (length x, head x)) . group
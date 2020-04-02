import System.IO
import System.Random
import Data.List

permutacje:: [a] -> IO [a]
permutacje xs = randomElement . permutations $ xs

randomElement :: [a] -> IO a
randomElement xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

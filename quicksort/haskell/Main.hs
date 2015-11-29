
import System.Random

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
  where
    lesser = [y | y <- xs, y < p]
    greater = [y | y <- xs, y >= p]


main :: IO ()
main = do
  g <- newStdGen
  print $ quicksort (take 10 (randomRs (0 :: Int, 100) g))

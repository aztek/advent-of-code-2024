import Data.Bifunctor (bimap, second)
import Data.List.NonEmpty (NonEmpty(..), fromList)

main :: IO ()
main = do
  input <- parse <$> getContents
  -- Part 1
  print $ solve [(+), (*)] input
  -- Part 2
  print $ solve [(+), (*), (||:)] input

parse :: String -> [(Int, NonEmpty Int)]
parse = map parseRow . lines
  where
    parseRow = bimap read (fromList . map read . words) . splitBy ':'
    splitBy sep = second tail . span (/= sep)

solutions :: [Int -> Int -> Int] -> NonEmpty Int -> [Int]
solutions ops (x :| xs) = solutions' [x] xs
  where
    solutions' sols [] = sols
    solutions' sols (x:xs) = solutions' [op sol x | op <- ops, sol <- sols] xs

(||:) :: Int -> Int -> Int
x ||: y = read (show x <> show y)

solve :: [Int -> Int -> Int] -> [(Int, NonEmpty Int)] -> Int
solve ops = sum . map fst . filter (\(r, xs) -> r `elem` solutions ops xs)

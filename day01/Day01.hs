import Data.List (sort)

main :: IO ()
main = do
  input <- unzip . map parseLine . lines <$> getContents
  -- Part 1
  print $ uncurry distance input
  -- Part 2
  print $ uncurry similarity input

parseLine :: String -> (Int, Int)
parseLine line = (read left, read right) where [left, right] = words line

distance :: [Int] -> [Int] -> Int
distance lefts rights = sum $ zipWith delta (sort lefts) (sort rights)
  where
    delta left right = abs (left - right)

similarity :: [Int] -> [Int] -> Int
similarity lefts rights = sum $ map score lefts
  where
    score left = left * occurrences left
    occurrences left = length $ filter (== left) rights

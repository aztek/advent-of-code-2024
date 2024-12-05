import Data.Bifunctor (bimap, second)
import Data.List (partition, permutations)

main :: IO ()
main = do
  (rules, updates) <- parse <$> getContents
  let (valids, invalids) = partition (isValid rules) updates
  -- Part 1
  print $ sum $ map middle valids
  -- Part 2
  print $ sum $ map (middle . reorder rules) invalids

type Rule = (Int, Int)

type Update = [Int]

parse :: String -> ([Rule], [Update])
parse = bimap (map parseRule) (map parseUpdate) . splitBy "" . lines
  where
    parseRule = bimap read read . splitBy '|'
    parseUpdate = map read . words . map (\c -> if c == ',' then ' ' else c)
    splitBy sep = second tail . span (/= sep)

isValid :: [Rule] -> Update -> Bool
isValid rules = null . violations rules

violations :: [Rule] -> Update -> [Rule]
violations rules update = filter (violates update) rules

violates :: Update -> Rule -> Bool
violates (x : xs) (a, b) = (x == b && a `elem` xs) || violates xs (a, b)
violates [] _ = False

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

reorder :: [Rule] -> Update -> Update
reorder rules update
  | rule : _ <- violations rules update = reorder rules (swap rule update)
  | otherwise = update

swap :: Rule -> Update -> Update
swap (a, b) = map (\c -> if c == a then b else if c == b then a else c)

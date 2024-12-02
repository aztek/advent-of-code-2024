main :: IO ()
main = do
  reports <- parseReports <$> getContents
  -- Part 1
  print $ length $ filter isSafeReport reports
  -- Part 2
  print $ length $ filter isSafishReport reports

type Report = [Int]

parseReports :: String -> [Report]
parseReports = map (map read . words) . lines

isSafeReport :: Report -> Bool
isSafeReport levels
  | Just differences <- mapM (uncurry gauge) (pairs levels) =
    all (== Increasing) differences || all (== Decreasing) differences
  | otherwise = False

isSafishReport :: Report -> Bool
isSafishReport report = isSafeReport report || any isSafeReport (shorts report)

shorts :: [a] -> [[a]]
shorts [] = []
shorts (a : as) = as : map (a :) (shorts as)

data Difference = Increasing | Decreasing
  deriving (Show, Eq)

gauge :: Int -> Int -> Maybe Difference
gauge a b
  | b - a >= 1 && b - a <= 3 = Just Increasing
  | a - b >= 1 && a - b <= 3 = Just Decreasing
  | otherwise = Nothing

pairs :: [a] -> [(a, a)]
pairs (a : b : cs) = (a, b) : pairs (b : cs)
pairs _ = []

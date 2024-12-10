import Data.Array.Unboxed
import Data.Bifunctor
import Data.Char

main :: IO ()
main = do
  disk <- parse <$> getContents
  -- Part 1
  print $ checksum $ format disk
  -- Part 2
  print $ checksum $ defragment disk

type Disk = Array Int Int

parse :: String -> Disk
parse input = listArray (0, length digits - 1) digits
  where
    digits = file 0 (map digitToInt input)

    file _ [] = []
    file i (x : xs) = replicate x i <> free (i + 1) xs

    free _ [] = []
    free i (x : xs) = replicate x (-1) <> file i xs

pretty :: Disk -> String
pretty = fmap (\c -> if c < 0 then '.' else intToDigit c) . elems

format :: Disk -> Disk
format disk = helper (bounds disk) disk
  where
    helper (i, j) acc | i > j = acc
    helper (i, j) acc = case (acc ! i, acc ! j) of
      (-1, -1) -> helper (i, j - 1) acc
      (-1, c) -> helper (i + 1, j - 1) (acc // [(i, c), (j, -1)])
      (c, _) -> helper (i + 1, j) acc

defragment :: Disk -> Disk
defragment disk = helper (bounds disk) disk
  where
    helper (i, j) acc
      | file == 0 = acc
      | free > file = helper (0, file - 1) acc
      | freeSize >= fileSize = helper (free + fileSize, file - 1) acc'
      | otherwise = helper (free + freeSize, file) acc
      where
        (free, file) = (nextFree i acc, prevFile j acc)
        (freeSize, fileSize) = (sizeFree free acc, sizeFile file acc)
        acc' = acc // concatMap swaps [0 .. fileSize - 1]
        swaps k = [(free + k, acc ! (file + k)), (file + k, acc ! (free + k))]

nextFree :: Int -> Disk -> Int
nextFree 0 disk = nextFree 1 disk
nextFree i disk
  | cur == -1, prev /= -1 = i
  | otherwise = nextFree (i + 1) disk
  where
    (cur, prev) = (disk ! i, disk ! (i - 1))

sizeFree :: Int -> Disk -> Int
sizeFree i = length . takeWhile (< 0) . drop i . elems

prevFile :: Int -> Disk -> Int
prevFile 0 disk = 0
prevFile i disk
  | cur /= -1, cur /= prev = i
  | otherwise = prevFile (i - 1) disk
  where
    (cur, prev) = (disk ! i, disk ! (i - 1))

sizeFile :: Int -> Disk -> Int
sizeFile i disk = length $ takeWhile (== head xs) xs
  where xs = drop i (elems disk)

checksum :: Disk -> Int
checksum = sum . map (\(i, c) -> if c < 0 then 0 else i * c) . assocs

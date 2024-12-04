import Data.List (sort, transpose)

main :: IO ()
main = do
  input <- lines <$> getContents
  -- Part 1
  print $ xmases input
  -- Part 2
  print $ xMases input

xmases :: [[Char]] -> Int
xmases input = horisontal input + vertical input + diagonal input + antidiagonal input

horisontal, vertical, diagonal, antidiagonal :: [[Char]] -> Int
horisontal = sum . map countXmas
vertical = horisontal . transpose
diagonal = vertical . pad
antidiagonal = diagonal . map reverse

pad :: [[Char]] -> [[Char]]
pad = zipWith (<>) (map (`replicate` '.') [0 ..])

countXmas :: [Char] -> Int
countXmas [] = 0
countXmas ('X' : 'M' : 'A' : 'S' : str) = 1 + countXmas ('S' : str)
countXmas ('S' : 'A' : 'M' : 'X' : str) = 1 + countXmas ('X' : str)
countXmas (_ : str) = countXmas str

xMases :: [[Char]] -> Int
xMases = length . filter isXmas . squares

type Row = (Char, Char, Char)

rows :: [Char] -> [Row]
rows (a : tl@(b : c : _)) = (a, b, c) : rows tl
rows _ = []

type Square = (Row, Row, Row)

squares :: [[Char]] -> [Square]
squares (a : tl@(b : c : _)) = zip3 (rows a) (rows b) (rows c) <> squares tl
squares _ = []

isXmas :: Square -> Bool
isXmas
  ( (tl, _, tr),
    (_, 'A', _),
    (bl, _, br)
    ) = sort [tl, tr, bl, br] == "MMSS" && tl /= br && tr /= bl
isXmas _ = False

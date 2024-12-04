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
pad = zipWith (<>) $ map (`replicate` '.') [0 ..]

countXmas :: [Char] -> Int
countXmas ('X' : 'M' : 'A' : tl@('S' : _)) = 1 + countXmas tl
countXmas ('S' : 'A' : 'M' : tl@('X' : _)) = 1 + countXmas tl
countXmas (_ : tl) = countXmas tl
countXmas [] = 0

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
    ) = sort [tl, br] == "MS" && sort [tr, bl] == "MS"
isXmas _ = False

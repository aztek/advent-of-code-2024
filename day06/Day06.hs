{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (guard)
import Data.Array (Array, (!), (//), array, inRange, bounds)
import Data.List (elemIndex, nub)
import Data.Set (Set, empty, member, insert)

main :: IO ()
main = do
  (area, loc@(_, pos)) <- parse <$> getContents
  let breadcrumbs = nub $ map snd (trace (next area) loc)
  -- Part 1
  print $ length breadcrumbs
  -- Part 2
  print $ length $ filter (\area -> loops (next area) loc) $ map (block area) $ filter (/= pos) breadcrumbs

type Area = Array (Int, Int) Bool

data Dir = U | R | D | L
  deriving (Show, Eq, Ord)

type Pos = (Int, Int)

type Loc = (Dir, Pos)

parse :: String -> (Area, Loc)
parse input = (area, (U, pos))
  where
    area = parseArea (lines input)
    pos = unsafeFirstJust $ withRows $ map ('^' `elemIndex`) (lines input)
    withRows = zipWith (\row -> fmap (row,)) [0..]

parseArea :: [String] -> Area
parseArea rows = array bounds values
  where
    bounds = ((0, 0), (length rows - 1, length (head rows) - 1))
    values = concat $ zipWith parseLine [0..] rows
    parseLine x = zipWith (\y b -> ((x, y), b)) [0..] . map (/= '#')

unsafeFirstJust :: [Maybe a] -> a
unsafeFirstJust = \case
  Just x : _ -> x
  _ : xs -> unsafeFirstJust xs
  _ -> error "no just"

rot :: Dir -> Dir
rot = \case
  U -> R
  R -> D
  D -> L
  L -> U

step :: Pos -> Dir -> Pos
step (x, y) = \case
  U -> (x - 1, y)
  R -> (x, y + 1)
  D -> (x + 1, y)
  L -> (x, y - 1)

next :: Area -> Loc -> Maybe Loc
next area (dir, pos) = do
  let pos' = step pos dir
  guard (inRange (bounds area) pos')
  return $ if area ! pos' then (dir, pos') else (rot dir, pos)

trace :: (a -> Maybe a) -> a -> [a]
trace f x = trace' [x] x
  where
    trace' xs = \case
      x | Just y <- f x -> trace' (y : xs) y
      _ -> xs

loops :: Ord a => (a -> Maybe a) -> a -> Bool
loops f = loops' empty
  where
    loops' set = \case
      x | Just y <- f x -> y `member` set || loops' (insert y set) y
      _ -> False

block :: Area -> Pos -> Area
block area pos = area // [(pos, False)]

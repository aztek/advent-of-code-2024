{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (liftM2)
import Data.Array
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (groupBy, nub, sortBy)
import Data.Maybe (isNothing, mapMaybe)

main :: IO ()
main = do
  map <- parse <$> getContents
  -- Part 1
  print $ length $ nub $ concatMap (antidotes map) $ antennaPairs map
  -- Part 2
  print $ length $ nub $ concatMap (antidotes' map) $ antennaPairs map

type Antenna = Char

type Pos = (Int, Int)

type AntennaMap = Array Pos (Maybe Antenna)

parse :: String -> AntennaMap
parse input = array bounds values
  where
    rows = lines input
    bounds = ((0, 0), (length rows - 1, length (head rows) - 1))
    values = concat $ zipWith parseLine [0 ..] rows
    parseLine x = zipWith (\y e -> ((x, y), parseChar e)) [0 ..]
    parseChar = \case '.' -> Nothing; c -> Just c

antennas :: AntennaMap -> [(Pos, Antenna)]
antennas = mapMaybe (\(pos, ant) -> fmap (pos,) ant) . assocs

antennaPairs :: AntennaMap -> [(Pos, Pos)]
antennaPairs = concatMap (pairs . map fst) . groupBy ((==) `on` snd) . sortBy (compare `on` snd) . antennas

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (x,) xs <> pairs xs
pairs _ = []

pairwise :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
pairwise (#) (x, y) (x', y') = (x # x', y # y')

antidotes :: AntennaMap -> (Pos, Pos) -> [Pos]
antidotes map (a, b) = filter vacant [a `add` d, b `sub` d]
  where
    vacant = inRange (bounds map)
    d = a `sub` b
    add = pairwise (+)
    sub = pairwise (-)

antidotes' :: AntennaMap -> (Pos, Pos) -> [Pos]
antidotes' map (a, b) = takeWhile vacant adds <> takeWhile vacant subs
  where
    vacant = inRange (bounds map)
    d = a `sub` b
    add = pairwise (+)
    sub = pairwise (-)
    adds = iterate (`add` d) a
    subs = iterate (`sub` d) b

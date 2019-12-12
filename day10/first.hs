#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.List (sortBy)
import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> Set (Int, Int)
parse =
  let
    toCoord c (r, v) = ((c, r), v)
    toRow (c, ls) = map (toCoord c) ls
    appendCoords = concatMap toRow . zip [0..] . map (zip [0..])
    isAsteroid (coord, kind) = kind == '#'
  in
  Set.fromAscList . map fst . filter isAsteroid . appendCoords . lines

sqrDist :: (Int, Int) -> (Int, Int) -> Int
sqrDist (x0, y0) (x1, y1) = (x1 - x0) ^ 2 + (y1 - y0) ^ 2

angle :: (Int, Int) -> (Int, Int) -> Float
angle (x0, y0) (x1, y1) = atan2 (fromIntegral (x1 - x0)) (fromIntegral (y1 - y0))

count :: (Int, Int) -> Set (Int, Int) -> Int
count source set = Set.size $ Set.map (angle source) $ Set.delete source set

sights :: Set (Int, Int) -> [(Int, (Int, Int))]
sights set =
  let compareFst (a, _) (b, _) = compare b a
  in sortBy compareFst $ map (\p -> (count p set, p)) $ Set.toList set

main :: IO ()
main = do
  set <- parse <$> getContents
  putStrLn $ show $ head $ sights set

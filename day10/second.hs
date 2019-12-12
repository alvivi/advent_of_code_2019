#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.List (sortBy, groupBy)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace as D

parse :: String -> Set (Int, Int)
parse =
  let
    toCoord c (r, v) = ((r, c), v)
    toRow (c, ls) = map (toCoord c) ls
    appendCoords = concatMap toRow . zip [0..] . map (zip [0..])
    isAsteroid (coord, kind) = kind == '#'
  in
  Set.fromAscList . map fst . filter isAsteroid . appendCoords . lines

sqrDist :: (Int, Int) -> (Int, Int) -> Int
sqrDist (x0, y0) (x1, y1) = (x1 - x0) ^ 2 + (y1 - y0) ^ 2

angle :: (Int, Int) -> (Int, Int) -> Rational
angle (x0, y0) (x1, y1) = toRational $ atan2 (fromIntegral (x1 - x0)) (fromIntegral (y1 - y0))

count :: (Int, Int) -> Set (Int, Int) -> Int
count source set = Set.size $ Set.map (angle source) $ Set.delete source set

sights :: Set (Int, Int) -> [(Int, (Int, Int))]
sights set =
  let compareFst (a, _) (b, _) = compare b a
  in sortBy compareFst $ map (\p -> (count p set, p)) $ Set.toList set

groupByAngle :: (Int, Int) -> Set (Int, Int) -> [[(Int, Int)]]
groupByAngle center set =
  let
    step acc point =
      let a = angle center point
      in case Map.lookup a acc of
        Nothing -> Map.insert a [point] acc
        Just list -> Map.insert a (point : list) acc
    angleMap = foldl step Map.empty $ Set.toList $ Set.delete center set
  in
    map (fromJust . flip Map.lookup angleMap) $ Map.keys angleMap

join :: [[a]] -> [[a]]
join list =
  let
    step [] [] [] acc = acc
    step [] [] result acc = result : acc
    step [] nextRound result acc = step nextRound [] [] (result : acc)
    step ([] : xs) nextRound result acc = step xs nextRound result acc
    step ((y : ys) : xs) nextRound result acc = step xs (ys : nextRound) (y : result) acc
  in
    reverse $ step list [] [] []

main :: IO ()
main = do
  set <- parse <$> getContents
  let (_, center) = head $ sights set
  let groups = groupByAngle center $ Set.delete center set
  let sorted = map (sortBy (\a b -> compare (sqrDist center a) (sqrDist center b))) groups
  let angle' c b = pi - (fromRational $ angle c b)
  let asteroids = concat $ map (map snd . sortBy (\(a, _) (b, _) -> compare a b) . map (\p -> (angle' center p, p))) $ join sorted
  putStrLn $ show $ (flip (!!) 199) asteroids

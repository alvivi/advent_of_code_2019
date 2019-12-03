#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers --package text --ghc-options -XOverloadedStrings

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Debug.Trace as D

type Point = (Int, Int)
type Line = (Point, Point)
type Canvas = Set (Int, Int)

trace :: [Line] -> Canvas
trace lines =
  let
    project ((x0, y0), (x1, y1)) | x0 < x1 && y0 == y1 = [(i, y0) | i <- [x0..x1]]
    project ((x0, y0), (x1, y1)) | x0 >= x1 && y0 == y1 = [(i, y0) | i <- [x1..x0]]
    project ((x0, y0), (x1, y1)) | y0 < y1 && x0 == x1 = [(x0, j) | j <- [y0..y1]]
    project ((x0, y0), (x1, y1)) | y0 >= y1 && x0 == x1 = [(x0, j) | j <- [y1..y0]]
  in
    foldl (flip Set.insert) Set.empty $ concat $ map project lines

intersections :: [[Line]] -> [Point]
intersections = Set.toList . Set.delete (0, 0) . foldl1 Set.intersection . map trace

parseLine :: Line -> String -> Line
parseLine (_, p@(x0, y0)) ('U':ds) = (p, (x0, y0 - read ds))
parseLine (_, p@(x0, y0)) ('R':ds) = (p, (x0 + read ds, y0))
parseLine (_, p@(x0, y0)) ('D':ds) = (p, (x0, y0 + read ds))
parseLine (_, p@(x0, y0)) ('L':ds) = (p, (x0 - read ds, y0))

walk :: [Line] -> Point -> Int
walk lines point =
  let
    step (((x0, y0), (x1, y1)):ls) (px, py) acc | x0 == px && y0 == py = acc
    step (((x0, y0), (x1, y1)):ls) p acc | x0 == x1 && y0 == y1 = step ls p acc
    step (((x0, y0), (x1, y1)):ls) p acc | x0 < x1 && y0 == y1 = step (((x0 + 1, y0), (x1, y1)):ls) p (acc + 1)
    step (((x0, y0), (x1, y1)):ls) p acc | x0 >= x1 && y0 == y1 = step (((x0 - 1, y0), (x1, y1)):ls) p (acc + 1)
    step (((x0, y0), (x1, y1)):ls) p acc | y0 < y1 && x0 == x1 = step (((x0, y0 + 1), (x1, y1)):ls) p (acc + 1)
    step (((x0, y0), (x1, y1)):ls) p acc | y0 >= y1 && x0 == x1 = step (((x0, y0 - 1), (x1, y1)):ls) p (acc + 1)
  in
    step lines point 0

main :: IO ()
main =
  let
    parseRow = map (T.unpack . T.strip) . T.splitOn "," . T.pack
    parseLines = tail . scanl parseLine ((0, 0), (0, 0))
  in do
    contents <- getContents
    let lines' = map (parseLines . parseRow) $ lines contents
    let intersections' = intersections lines'
    putStrLn $ show $ minimum $ map (\i -> sum $ map (flip walk i) lines') intersections'

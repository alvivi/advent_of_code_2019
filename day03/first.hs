#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers --package text --ghc-options -XOverloadedStrings

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Debug.Trace as D

type Point = (Int, Int)
type Line = (Point, Point)
type Canvas = Set (Int, Int)

dst :: Point -> Int
dst (x, y) = abs x + abs y

trace :: [Line] -> Canvas
trace lines =
  let
    project ((x0, y0), (x1, y1)) | x0 < x1 && y0 == y1 = [(i, y0) | i <- [x0..x1]]
    project ((x0, y0), (x1, y1)) | x0 >= x1 && y0 == y1 = [(i, y0) | i <- [x1..x0]]
    project ((x0, y0), (x1, y1)) | y0 < y1 && x0 == x1 = [(x0, j) | j <- [y0..y1]]
    project ((x0, y0), (x1, y1)) | y0 >= y1 && x0 == x1 = [(x0, j) | j <- [y1..y0]]
  in
    foldl (flip Set.insert) Set.empty $ concat $ map project lines

findClosets :: [[Line]] -> Point
findClosets =
  let find p1 p2 = if dst p1 < dst p2 then p1 else p2
  in Set.foldl find (10000, 10000) . Set.delete (0, 0) . foldl1 Set.intersection . map trace

parseLine :: Line -> String -> Line
parseLine (_, p@(x0, y0)) ('U':ds) = (p, (x0, y0 - read ds))
parseLine (_, p@(x0, y0)) ('R':ds) = (p, (x0 + read ds, y0))
parseLine (_, p@(x0, y0)) ('D':ds) = (p, (x0, y0 + read ds))
parseLine (_, p@(x0, y0)) ('L':ds) = (p, (x0 - read ds, y0))

main :: IO ()
main =
  let
    parseRow = map (T.unpack . T.strip) . T.splitOn "," . T.pack
    parseLines = tail . scanl parseLine ((0, 0), (0, 0))
  in do
    contents <- getContents
    putStrLn $ show $ dst $ findClosets $ map (parseLines . parseRow) $ lines contents

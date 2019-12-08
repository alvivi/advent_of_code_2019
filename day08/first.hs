#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import Data.Char (isSpace)
import Data.List (sortBy)

main :: IO ()
main =
  let
    layerSize = 25 * 6
    trim = f . f where f = reverse . dropWhile isSpace
    parse = split . map (read . (: [])) . trim
    split []   = []
    split list = let (xs, ys) = splitAt layerSize list in xs : split ys
    countZeros list = (length $ filter (== 0) list, list)
    sort = sortBy (\(a, _) (b, _) -> compare a b)

    computeResult list = length (filter (== 1) list) * length (filter (== 2) list)
  in do
    contents <- getContents
    putStrLn $ show $ computeResult $ snd $ head $ sort $ map countZeros $ parse contents

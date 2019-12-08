#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import Data.Char (isSpace)
import Data.List (transpose)

blend :: [Int] -> Int
blend (2:cs) = blend cs
blend (c:_) = c

render :: [Int] -> String
render (0:cs) = '.' : render cs
render (1:cs) = 'X' : render cs
render (2:cs) = ' ' : render cs
render _ = []

main :: IO ()
main =
  let
    imageSize = (25, 6)
    layerSize = fst imageSize * snd imageSize
    trim = f . f where f = reverse . dropWhile isSpace
    split n []   = []
    split n list = let (xs, ys) = splitAt n list in xs : split n ys
    parse :: String -> [[Int]]
    parse = split layerSize . map (read . (: [])) . trim
  in do
    contents <- getContents
    mapM_ (putStrLn . render) $ split (fst imageSize) $ map blend $ transpose $ parse contents

#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Tree (Tree)
import qualified Data.Tree as Tree

count :: Tree String -> Int
count = sum . map (\(p, ls) -> p * length ls) . zip [0..] . Tree.levels

buildTree :: [(String, String)] -> Tree String
buildTree pairs =
  let step node = (node, map snd $ filter ((== node) . fst) pairs)
  in Tree.unfoldTree step "COM"

parse :: String -> (String, String)
parse line = let (lhs, rhs) = break (== ')') line in (lhs, tail rhs)

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ count $ buildTree $ map parse $ lines contents

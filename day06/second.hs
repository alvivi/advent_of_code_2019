#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

{-# LANGUAGE NamedFieldPuns #-}

import Data.Tree (Tree(Node), rootLabel, subForest)
import qualified Data.Tree as Tree
import Data.Maybe (fromJust)
import Data.List ((\\))

parents :: String -> Tree String -> Maybe [String]
parents label (Node {rootLabel}) | label == rootLabel = Just []
parents _     (Node {subForest}) | subForest == [] = Nothing
parents label (Node {rootLabel, subForest}) =
  let path = mconcat $ map (parents label) subForest
  in fmap ((:) rootLabel) path

buildTree :: [(String, String)] -> Tree String
buildTree pairs =
  let step node = (node, map snd $ filter ((== node) . fst) pairs)
  in Tree.unfoldTree step "COM"

parse :: String -> (String, String)
parse line = let (lhs, rhs) = break (== ')') line in (lhs, tail rhs)

main :: IO ()
main = do
  contents <- getContents
  let tree = buildTree $ map parse $ lines contents
  let pathToSan = fromJust $ parents "SAN" tree
  let pathToYou = fromJust $ parents "YOU" tree
  putStrLn $ show $ length (pathToSan \\ pathToYou) + length (pathToYou \\ pathToSan)

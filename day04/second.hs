#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import Data.List (group)

check :: Int -> Bool
check number =
  let
    digitList = digits number
    pairList = zip digitList $ tail digitList
    ordered = all (uncurry (<=)) pairList
    repeated = elem 2 $ map length $ group digitList
  in
    repeated && ordered

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits q ++ [r] where (q, r) = quotRem x 10

main :: IO ()
main =
  let count acc number = if check number then acc + 1 else acc
  in putStrLn $ show $ foldl count 0 [152085..670283]

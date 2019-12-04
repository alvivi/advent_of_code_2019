#!/usr/bin/env stack
-- stack script --resolver lts-14.16

check :: Int -> Bool
check number =
  let
    digitList = digits number
    pairList = zip digitList $ tail digitList
    repated = any (uncurry (==)) pairList
    ordered = all (uncurry (<=)) pairList
  in
    repated && ordered

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits q ++ [r] where (q, r) = quotRem x 10

main :: IO ()
main =
  let count acc number = if check number then acc + 1 else acc
  in putStrLn $ show $ foldl count 0 [152085..670283]

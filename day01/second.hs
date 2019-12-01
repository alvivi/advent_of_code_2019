#!/usr/bin/env stack
-- stack script --resolver lts-14.16

computeFuel :: Integer -> Integer
computeFuel mass =
  let fuel = floor (fromInteger mass / 3.0) - 2
  in if fuel > 8 then fuel + computeFuel fuel else fuel

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ sum $ map (computeFuel . read) $ lines contents

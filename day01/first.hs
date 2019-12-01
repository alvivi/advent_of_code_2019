#!/usr/bin/env stack
-- stack script --resolver lts-14.16

computeFuel :: Integer -> Integer
computeFuel mass = floor (fromInteger mass / 3.0) - 2

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ sum $ map (computeFuel . read) $ lines contents

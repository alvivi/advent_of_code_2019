#!/usr/bin/env stack
-- stack script --resolver lts-14.16

moons :: [((Int, Int, Int), (Int, Int, Int))]
moons =
  [ ((-2,  9, -5), (0, 0, 0))
  , ((16, 19,  9), (0, 0, 0))
  , (( 0,  3,  6), (0, 0, 0))
  , ((11,  0, 11), (0, 0, 0))
  ]

gravity :: Int -> [Int] -> Int
gravity source bodies =
  let vel a b = (fromEnum $ compare b a) - 1
  in foldl (\acc target -> acc + vel source target) 0 bodies

step :: [(Int, Int)] -> [(Int, Int)]
step list =
  let
    step' (position, velocity) =
      let velocity' = velocity + gravity position (map fst list)
      in (position + velocity', velocity')
  in
    map step' list

findPeriod :: [(Int, Int)] -> Int
findPeriod list =
  let find count state = if state == list then count else find (count + 1) $ step state
  in find 1 (step list)

getX :: (a, b, c) -> a
getX (a, _, _) = a

getY :: (a, b, c) -> b
getY (_, b, _) = b

getZ :: (a, b, c) -> c
getZ (_, _, c) = c

applyBoth ::  (a -> b) -> (a, a) -> (b, b)
applyBoth fn (a, b) = (fn a, fn b)

main :: IO ()
main =
  let
    xp = findPeriod $ map (applyBoth getX) moons
    yp = findPeriod $ map (applyBoth getY) moons
    zp = findPeriod $ map (applyBoth getZ) moons

  in do
    putStrLn $ show $ lcm (lcm xp yp) zp

#!/usr/bin/env stack
-- stack script --resolver lts-14.16

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)

moons :: [(Position, Velocity)]
moons =
  [ ((-2,  9, -5), (0, 0, 0))
  , ((16, 19,  9), (0, 0, 0))
  , (( 0,  3,  6), (0, 0, 0))
  , ((11,  0, 11), (0, 0, 0))
  ]

add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

gravity :: Position -> [Position] -> Velocity
gravity source bodies =
  let
    vel a b = (fromEnum $ compare b a) - 1
    gravity' (x0, y0, z0) (x1, y1, z1) = (vel x0 x1, vel y0 y1, vel z0 z1)
  in
    foldl (\acc target -> add acc $ gravity' source target) (0, 0, 0) bodies

step :: [(Position, Velocity)] -> [(Position, Velocity)]
step list =
  let
    step' (position, velocity) =
      let velocity' =  add velocity $ gravity position $ map fst list
      in (add position velocity', velocity')
  in
    map step' list

simulate :: Int -> [(Position, Velocity)] -> [(Position, Velocity)]
simulate 0 list = list
simulate count list = simulate (count - 1) $ step list

absSum :: (Int, Int, Int) -> Int
absSum (x, y, z) = abs x + abs y + abs z

energy :: [(Position, Velocity)] -> Int
energy =
  let energy' (position, velocity) = absSum position * absSum velocity
  in sum . map energy'

main :: IO ()
main = do
  putStrLn $ show $ energy $ simulate 1000 moons

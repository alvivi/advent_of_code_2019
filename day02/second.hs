#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package array --package text --ghc-options -XOverloadedStrings

import Data.Array (Array, (!), (//))
import Data.Either (fromRight)
import Data.Text.Read (decimal)
import qualified Data.Array as A
import qualified Data.Text as T

runProgram :: Array Int Int -> Array Int Int
runProgram source =
  let
    step pc source =
      let
        lhsIndex = source ! (pc + 1)
        lhs = source ! lhsIndex
        rhsIndex = source ! (pc + 2)
        rhs = source ! rhsIndex
        targetIndex = source ! (pc + 3)
      in
        case source ! pc of
          1 -> step (pc + 4) (source // [(targetIndex, lhs + rhs)])
          2 -> step (pc + 4) (source // [(targetIndex, lhs * rhs)])
          99 -> source
          _ -> error "unknown opcode"
  in
    step 0 source

fixSource :: (Int, Int) -> Array Int Int -> Array Int Int
fixSource (noum, verb) source = source // [(1, noum), (2, verb)]

main :: IO ()
main =
  let
    parse = map (fst . fromRight (-1, "") . decimal . T.strip) . T.splitOn ","
    buildArray list = A.listArray (0, length list - 1) list
  in do
    contents <- T.pack <$> getContents
    let source = buildArray $ parse contents
    flip mapM_ [(i, j) | i <- [0..99], j <- [0..99]] $ \ input ->
      putStrLn $ show $ runProgram $ fixSource input source

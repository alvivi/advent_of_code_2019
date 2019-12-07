#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package array --package text --package containers

{-# LANGUAGE OverloadedStrings #-}

import Data.Array (Array, (!), (//))
import Data.Either (fromRight)
import Data.List (permutations, sortBy)
import Data.Map.Lazy (Map)
import Data.Text.Read (signed, decimal)
import qualified Data.Array as A
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T

import Debug.Trace

readOpcode :: Int -> (Int, [Int])
readOpcode int =
  let
    digits x = r : digits q where (q, r) = quotRem x 10
    (modes, opcode) = quotRem int 100
  in
    (opcode, take 4 $ digits modes ++ repeat 0)

runProgram :: Array Int Int -> [Int] -> (Array Int Int, [Int])
runProgram source input =
  let
    step pc input (source, output) =
      let
        lhsIndex = source ! (pc + 1)
        lhs = source ! lhsIndex
        rhsIndex = source ! (pc + 2)
        rhs = source ! rhsIndex
        targetIndex = source ! (pc + 3)
      in
        case readOpcode (source ! pc) of
          (1, (0:0:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhs + rhs)], output)
          (1, (1:0:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhsIndex + rhs)], output)
          (1, (0:1:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhs + rhsIndex)], output)
          (1, (1:1:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhsIndex + rhsIndex)], output)
          (1, (_:_:1:_)) -> error "invalid immediate mode at target in add op"
          (2, (0:0:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhs * rhs)], output)
          (2, (1:0:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhsIndex * rhs)], output)
          (2, (0:1:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhs * rhsIndex)], output)
          (2, (1:1:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhsIndex * rhsIndex)], output)
          (2, (_:_:1:_)) -> error "invalid immediate mode at target in mul op"
          (3, (0:_)) -> step (pc + 2) (tail input) (source // [(lhsIndex, head input)], output)
          (3, (1:_)) -> error "invalid immediate mode at target in read op"
          (4, (0:_)) -> step (pc + 2) input (source, lhs:output)
          (4, (1:_)) -> step (pc + 2) input (source, lhsIndex:output)
          (5, (0:0:_)) -> if lhs /= 0 then step rhs input (source, output) else step (pc + 3) input (source, output)
          (5, (1:0:_)) -> if lhsIndex /= 0 then step rhs input (source, output) else step (pc + 3) input (source, output)
          (5, (0:1:_)) -> if lhs /= 0 then step rhsIndex input (source, output) else step (pc + 3) input (source, output)
          (5, (1:1:_)) -> if lhsIndex /= 0 then step rhsIndex input (source, output) else step (pc + 3) input (source, output)
          (6, (0:0:_)) -> if lhs == 0 then step rhs input (source, output) else step (pc + 3) input (source, output)
          (6, (1:0:_)) -> if lhsIndex == 0 then step rhs input (source, output) else step (pc + 3) input (source, output)
          (6, (0:1:_)) -> if lhs == 0 then step rhsIndex input (source, output) else step (pc + 3) input (source, output)
          (6, (1:1:_)) -> if lhsIndex == 0 then step rhsIndex input (source, output) else step (pc + 3) input (source, output)
          (7, (0:0:0:_)) -> let res = if lhs < rhs then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)], output)
          (7, (1:0:0:_)) -> let res = if lhsIndex < rhs then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)], output)
          (7, (0:1:0:_)) -> let res = if lhs < rhsIndex then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)], output)
          (7, (1:1:0:_)) -> let res = if lhsIndex < rhsIndex then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)], output)
          (7, (_:_:1:_)) -> error "invalid immediate mode at target in less than op"
          (8, (0:0:0:_)) -> let res = if lhs == rhs then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)], output)
          (8, (1:0:0:_)) -> let res = if lhsIndex == rhs then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)], output)
          (8, (0:1:0:_)) -> let res = if lhs == rhsIndex then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)], output)
          (8, (1:1:0:_)) -> let res = if lhsIndex == rhsIndex then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)], output)
          (8, (_:_:1:_)) -> error "invalid immediate mode at target in equal op"
          (99, _) -> (source, output)
  in
    step 0 input (source, [])

type Cache = Map [Int] [Int]

runProgramCached :: Cache -> [Int] -> Array Int Int -> (Cache, [Int])
runProgramCached cache input source =
  case Map.lookup input cache of
    Just result ->
      (cache, result)
    Nothing ->
      let (_, result) = runProgram source input
      in (Map.insert input result cache, result)

runPhases :: Cache -> [Int] -> Array Int Int -> (Cache, Int)
runPhases cache pashes source =
  let
    step input cache [] = (cache, input)
    step input cache (phase : nextPhases) =
      let (nextCache, result) = runProgramCached cache [phase, input] source
      in step (head result) nextCache nextPhases
  in step 0 cache pashes

computePermutations :: Array Int Int -> [(Int, [Int])]
computePermutations source =
  let
    step (cache, acc) phase =
      let (nextCache, result) = runPhases cache phase source
      in (nextCache, (result, phase) : acc)
  in
    snd $ foldl step (Map.empty, []) $ permutations [0..4]

main :: IO ()
main =
  let
    parse = map (fst . fromRight (-1, "") . signed decimal . T.strip) . T.splitOn ","
    buildArray list = A.listArray (0, length list - 1) list
    sort = sortBy (\(a, _) (b, _) -> compare b a)
  in do
    contents <- T.pack <$> getContents
    putStrLn $ show $ head $ sort $ computePermutations $ buildArray $ parse contents

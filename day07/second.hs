#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package array --package text --package containers

{-# LANGUAGE OverloadedStrings #-}

import Data.Array (Array, (!), (//))
import Data.Either (fromRight)
import Data.List (permutations, sortBy)
import Data.Map.Lazy (Map)
import Data.Maybe (fromJust)
import Data.Text.Read (signed, decimal)
import qualified Data.Array as A
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T

readOpcode :: Int -> (Int, [Int])
readOpcode int =
  let
    digits x = r : digits q where (q, r) = quotRem x 10
    (modes, opcode) = quotRem int 100
  in
    (opcode, take 4 $ digits modes ++ repeat 0)

type Source = Array Int Int
type State = (Source, Int, Maybe Int)

runProgram :: Source -> Int -> [Int] -> State
runProgram source pc input =
  let
    step pc input source  =
      let
        lhsIndex = source ! (pc + 1)
        lhs = source ! lhsIndex
        rhsIndex = source ! (pc + 2)
        rhs = source ! rhsIndex
        targetIndex = source ! (pc + 3)
      in
        case readOpcode (source ! pc) of
          (1, (0:0:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhs + rhs)])
          (1, (1:0:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhsIndex + rhs)])
          (1, (0:1:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhs + rhsIndex)])
          (1, (1:1:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhsIndex + rhsIndex)])
          (1, (_:_:1:_)) -> error "invalid immediate mode at target in add op"
          (2, (0:0:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhs * rhs)])
          (2, (1:0:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhsIndex * rhs)])
          (2, (0:1:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhs * rhsIndex)])
          (2, (1:1:0:_)) -> step (pc + 4) input (source // [(targetIndex, lhsIndex * rhsIndex)])
          (2, (_:_:1:_)) -> error "invalid immediate mode at target in mul op"
          (3, (0:_)) -> step (pc + 2) (tail input) (source // [(lhsIndex, head input)])
          (3, (1:_)) -> error "invalid immediate mode at target in read op"
          (4, (0:_)) -> (source, pc + 2, Just lhs)
          (4, (1:_)) -> (source, pc + 2, Just lhsIndex)
          (5, (0:0:_)) -> if lhs /= 0 then step rhs input source else step (pc + 3) input (source)
          (5, (1:0:_)) -> if lhsIndex /= 0 then step rhs input source else step (pc + 3) input source
          (5, (0:1:_)) -> if lhs /= 0 then step rhsIndex input source else step (pc + 3) input source
          (5, (1:1:_)) -> if lhsIndex /= 0 then step rhsIndex input source else step (pc + 3) input source
          (6, (0:0:_)) -> if lhs == 0 then step rhs input source else step (pc + 3) input source
          (6, (1:0:_)) -> if lhsIndex == 0 then step rhs input source else step (pc + 3) input source
          (6, (0:1:_)) -> if lhs == 0 then step rhsIndex input source else step (pc + 3) input source
          (6, (1:1:_)) -> if lhsIndex == 0 then step rhsIndex input source else step (pc + 3) input source
          (7, (0:0:0:_)) -> let res = if lhs < rhs then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)])
          (7, (1:0:0:_)) -> let res = if lhsIndex < rhs then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)])
          (7, (0:1:0:_)) -> let res = if lhs < rhsIndex then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)])
          (7, (1:1:0:_)) -> let res = if lhsIndex < rhsIndex then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)])
          (7, (_:_:1:_)) -> error "invalid immediate mode at target in less than op"
          (8, (0:0:0:_)) -> let res = if lhs == rhs then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)])
          (8, (1:0:0:_)) -> let res = if lhsIndex == rhs then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)])
          (8, (0:1:0:_)) -> let res = if lhs == rhsIndex then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)])
          (8, (1:1:0:_)) -> let res = if lhsIndex == rhsIndex then 1 else 0 in step (pc + 4) input (source // [(targetIndex, res)])
          (8, (_:_:1:_)) -> error "invalid immediate mode at target in equal op"
          (99, _) -> (source, pc, Nothing)
  in
    step pc input source

runPhase :: [Int] -> Source -> Int
runPhase phases source =
  let
    initialInputs = map (uncurry (:)) (zip phases ([0] : cycle [[]]))
    initialStates = repeat (source, 0, Nothing)
    step index states =
      let
        ((source, pc, _), inputs) = states ! index
        (newSource, newPc, maybeOutput) = runProgram source pc inputs
      in
        case maybeOutput of
          Just output ->
            let
              nextIndex = mod (index + 1) (length states)
              (nextState, nextInputs) = states ! nextIndex
              newStates = states //
                [ (nextIndex, (nextState, nextInputs ++ [output]))
                , (index, ((newSource, newPc, maybeOutput), []))
                ]
            in
              step nextIndex newStates
          Nothing ->
            let ((_, _, lastOutput), _) = states ! (length states - 1)
            in fromJust lastOutput
  in
    step 0 $ A.listArray (0, length phases - 1) $ zip initialStates initialInputs

computePermutations :: Source -> [(Int, [Int])]
computePermutations source =
  let
    step acc phase =
      let result = runPhase phase source
      in (result, phase) : acc
  in
    foldl step [] $ permutations [5..9]


main :: IO ()
main =
  let
    parse = map (fst . fromRight (-1, "") . signed decimal . T.strip) . T.splitOn ","
    buildArray list = A.listArray (0, length list - 1) list
    sort = sortBy (\(a, _) (b, _) -> compare b a)
  in do
    contents <- T.pack <$> getContents
    putStrLn $ show $ head $ sort $ computePermutations $ buildArray $ parse contents

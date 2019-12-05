#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package array --package text

{-# LANGUAGE OverloadedStrings #-}

import Data.Array (Array, (!), (//))
import Data.Either (fromRight)
import Data.Text.Read (signed, decimal)
import qualified Data.Array as A
import qualified Data.Text as T

readOpcode :: Int -> (Int, [Int])
readOpcode int =
  let
    digits x = r : digits q where (q, r) = quotRem x 10
    (modes, opcode) = quotRem int 100
  in
    (opcode, take 4 $ digits modes ++ repeat 0)

runProgram :: Array Int Int -> (Array Int Int, [Int])
runProgram source =
  let
    step pc (source, output) =
      let
        lhsIndex = source ! (pc + 1)
        lhs = source ! lhsIndex
        rhsIndex = source ! (pc + 2)
        rhs = source ! rhsIndex
        targetIndex = source ! (pc + 3)
      in
        case readOpcode (source ! pc) of
          (1, (0:0:0:_)) -> step (pc + 4) (source // [(targetIndex, lhs + rhs)], output)
          (1, (1:0:0:_)) -> step (pc + 4) (source // [(targetIndex, lhsIndex + rhs)], output)
          (1, (0:1:0:_)) -> step (pc + 4) (source // [(targetIndex, lhs + rhsIndex)], output)
          (1, (1:1:0:_)) -> step (pc + 4) (source // [(targetIndex, lhsIndex + rhsIndex)], output)
          (1, (_:_:1:_)) -> error "invalid immediate mode at target in add op"
          (2, (0:0:0:_)) -> step (pc + 4) (source // [(targetIndex, lhs * rhs)], output)
          (2, (1:0:0:_)) -> step (pc + 4) (source // [(targetIndex, lhsIndex * rhs)], output)
          (2, (0:1:0:_)) -> step (pc + 4) (source // [(targetIndex, lhs * rhsIndex)], output)
          (2, (1:1:0:_)) -> step (pc + 4) (source // [(targetIndex, lhsIndex * rhsIndex)], output)
          (2, (_:_:1:_)) -> error "invalid immediate mode at target in mul op"
          (3, (0:_)) -> step (pc + 2) (source // [(targetIndex, 5)], output)
          (3, (1:_)) -> error "invalid immediate mode at target in read op"
          (4, (0:_)) -> step (pc + 2) (source, lhs:output)
          (4, (1:_)) -> step (pc + 2) (source, lhsIndex:output)
          (5, (0:0:_)) -> if lhs /= 0 then step rhs (source, output) else step (pc + 3) (source, output)
          (5, (1:0:_)) -> if lhsIndex /= 0 then step rhs (source, output) else step (pc + 3) (source, output)
          (5, (0:1:_)) -> if lhs /= 0 then step rhsIndex (source, output) else step (pc + 3) (source, output)
          (5, (1:1:_)) -> if lhsIndex /= 0 then step rhsIndex (source, output) else step (pc + 3) (source, output)
          (6, (0:0:_)) -> if lhs == 0 then step rhs (source, output) else step (pc + 3) (source, output)
          (6, (1:0:_)) -> if lhsIndex == 0 then step rhs (source, output) else step (pc + 3) (source, output)
          (6, (0:1:_)) -> if lhs == 0 then step rhsIndex (source, output) else step (pc + 3) (source, output)
          (6, (1:1:_)) -> if lhsIndex == 0 then step rhsIndex (source, output) else step (pc + 3) (source, output)
          (7, (0:0:0:_)) -> let res = if lhs < rhs then 1 else 0 in step (pc + 4) (source // [(targetIndex, res)], output)
          (7, (1:0:0:_)) -> let res = if lhsIndex < rhs then 1 else 0 in step (pc + 4) (source // [(targetIndex, res)], output)
          (7, (0:1:0:_)) -> let res = if lhs < rhsIndex then 1 else 0 in step (pc + 4) (source // [(targetIndex, res)], output)
          (7, (1:1:0:_)) -> let res = if lhsIndex < rhsIndex then 1 else 0 in step (pc + 4) (source // [(targetIndex, res)], output)
          (7, (_:_:1:_)) -> error "invalid immediate mode at target in less than op"
          (8, (0:0:0:_)) -> let res = if lhs == rhs then 1 else 0 in step (pc + 4) (source // [(targetIndex, res)], output)
          (8, (1:0:0:_)) -> let res = if lhsIndex == rhs then 1 else 0 in step (pc + 4) (source // [(targetIndex, res)], output)
          (8, (0:1:0:_)) -> let res = if lhs == rhsIndex then 1 else 0 in step (pc + 4) (source // [(targetIndex, res)], output)
          (8, (1:1:0:_)) -> let res = if lhsIndex == rhsIndex then 1 else 0 in step (pc + 4) (source // [(targetIndex, res)], output)
          (8, (_:_:1:_)) -> error "invalid immediate mode at target in equal op"
          (99, _) -> (source, output)
  in
    step 0 (source, [])

main :: IO ()
main =
  let
    parse = map (fst . fromRight (-1, "") . signed decimal . T.strip) . T.splitOn ","
    buildArray list = A.listArray (0, length list - 1) list
  in do
    contents <- T.pack <$> getContents
    putStrLn $ show $ snd $ runProgram $ buildArray $ parse contents

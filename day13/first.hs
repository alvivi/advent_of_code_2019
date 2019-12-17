#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package array --package text --package containers

{-# LANGUAGE OverloadedStrings #-}

import Data.Either (fromRight)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
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
    (opcode, digits modes ++ repeat 0)

type Source = Map Int Int

runProgram :: Source -> [Int] -> (Source, [Int])
runProgram source input =
  let
    step pc rb input (source, output) =
      let
        readFromSource index source = fromMaybe 0 $ Map.lookup index source
        write = Map.insert
        lhsIndex = readFromSource (pc + 1) source
        lhs = readFromSource lhsIndex source
        lhsRelative = readFromSource (rb + lhsIndex) source
        rhsIndex = readFromSource (pc + 2) source
        rhs = readFromSource rhsIndex source
        rhsRelative = readFromSource (rb + rhsIndex) source
        targetIndex 0 = readFromSource (pc + 3) source
        targetIndex 2 = rb + targetIndex 0
        targetIndex lol = error $ show lol
        lhsValue 0 = lhs
        lhsValue 1 = lhsIndex
        lhsValue 2 = lhsRelative
        rhsValue 0 =  rhs
        rhsValue 1 = rhsIndex
        rhsValue 2 = rhsRelative
      in
        case readOpcode (readFromSource pc source) of
          (1, (_:_:1:_)) -> error "invalid immediate mode at target in add op"
          (1, (l:r:t:_)) -> step (pc + 4) rb input (write (targetIndex t) (lhsValue l + rhsValue r) source, output)
          (2, (_:_:1:_)) -> error "invalid immediate mode at target in mul op"
          (2, (l:r:t:_)) -> step (pc + 4) rb input (write (targetIndex t) (lhsValue l * rhsValue r) source, output)
          (3, (1:_)) -> error "invalid immediate mode at target in read op"
          (3, (0:_)) -> step (pc + 2) rb (tail input) (write lhsIndex (head input) source, output)
          (3, (2:_)) -> step (pc + 2) rb (tail input) (write (rb + lhsIndex) (head input) source, output)
          (4, (l:_)) -> step (pc + 2) rb input (source, (lhsValue l):output)
          (5, (l:r:_)) -> if lhsValue l /= 0 then step (rhsValue r) rb input (source, output) else step (pc + 3) rb input (source, output)
          (6, (l:r:_)) -> if lhsValue l == 0 then step (rhsValue r) rb input (source, output) else step (pc + 3) rb input (source, output)
          (7, (_:_:1:_)) -> error "invalid immediate mode at target in less than op"
          (7, (l:r:t:_)) -> let res = if lhsValue l < rhsValue r then 1 else 0 in step (pc + 4) rb input (write (targetIndex t) res source, output)
          (8, (_:_:1:_)) -> error "invalid immediate mode at target in equal op"
          (8, (l:r:t:_)) -> let res = if lhsValue l == rhsValue r then 1 else 0 in step (pc + 4) rb input (write (targetIndex t) res source, output)
          (9, (l:_)) -> step (pc + 2) (rb + lhsValue l) input (source, output)
          (99, _) -> (source, output)
  in
    step 0 0 input (source, [])

render :: [Int] -> Map (Int, Int) Int
render inputs =
  let
    step map [] = map
    step map (x : y : unit : next) = step (Map.insert (x, y) unit map) next
  in
    step Map.empty inputs

main :: IO ()
main =
  let
    parse = map (fst . fromRight (-1, "") . signed decimal . T.strip) . T.splitOn ","
    buildSource = foldl (flip (uncurry Map.insert)) Map.empty . zip [0..]
    count = Map.size . Map.filter (== 2)
  in do
    source <- buildSource <$> parse <$> T.pack <$> getContents
    putStrLn $ show $ count $ render $ reverse $ snd $ flip runProgram [2] source

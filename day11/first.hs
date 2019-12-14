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

type Memory = Map Int Int

readOpcode :: Int -> (Int, [Int])
readOpcode int =
  let
    digits x = r : digits q where (q, r) = quotRem x 10
    (modes, opcode) = quotRem int 100
  in
    (opcode, digits modes ++ repeat 0)

runInstruction :: Int -> Int -> [Int] -> Memory -> (Int, Int, [Int], Memory, Maybe Int, Bool)
runInstruction pc rb input source =
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
      (1, (l:r:t:_)) -> (pc + 4, rb, input, write (targetIndex t) (lhsValue l + rhsValue r) source, Nothing, True)
      (2, (_:_:1:_)) -> error "invalid immediate mode at target in mul op"
      (2, (l:r:t:_)) -> (pc + 4, rb, input, write (targetIndex t) (lhsValue l * rhsValue r) source, Nothing, True)
      (3, (1:_)) -> error "invalid immediate mode at target in read op"
      (3, (0:_)) -> (pc + 2, rb, tail input, write lhsIndex (head input) source, Nothing, True)
      (3, (2:_)) -> (pc + 2, rb, tail input, write (rb + lhsIndex) (head input) source, Nothing, True)
      (4, (l:_)) -> (pc + 2, rb, input, source, Just $ lhsValue l, True)
      (5, (l:r:_)) -> if lhsValue l /= 0 then (rhsValue r, rb, input, source, Nothing, True) else (pc + 3, rb, input, source, Nothing, True)
      (6, (l:r:_)) -> if lhsValue l == 0 then (rhsValue r, rb, input, source, Nothing, True) else (pc + 3, rb, input, source, Nothing, True)
      (7, (_:_:1:_)) -> error "invalid immediate mode at target in less than op"
      (7, (l:r:t:_)) -> let res = if lhsValue l < rhsValue r then 1 else 0 in (pc + 4, rb, input, write (targetIndex t) (res) source, Nothing, True)
      (8, (_:_:1:_)) -> error "invalid immediate mode at target in equal op"
      (8, (l:r:t:_)) -> let res = if lhsValue l == rhsValue r then 1 else 0 in (pc + 4, rb, input, write (targetIndex t) (res) source, Nothing, True)
      (9, (l:_)) -> (pc + 2, rb + lhsValue l, input, source, Nothing, True)
      (99, _) -> (pc, rb, input, source, Nothing, False)

data Direction = DUp | DRight | DDown | DLeft

move :: Direction -> (Int, Int) -> (Int, Int)
move DUp (x, y) = (x, y - 1)
move DRight (x, y) = (x + 1, y)
move DDown (x, y) = (x, y + 1)
move DLeft (x, y) = (x - 1, y)

turn :: Direction -> Int -> Direction
turn DUp 0 = DLeft
turn DRight 0 = DUp
turn DDown 0 = DRight
turn DLeft 0 = DDown
turn DUp 1 = DRight
turn DRight 1 = DDown
turn DDown 1 = DLeft
turn DLeft 1 = DUp

runProgram :: Memory -> Map (Int, Int) Int
runProgram source =
  let
    step position direction pc rb outputs panel source =
      let input = [fromMaybe 0 $ Map.lookup position panel]
      in case runInstruction pc rb input source of
        (nextPc, nextRb, _, nextSource, output, False) ->
          panel
        (nextPc, nextRb, _, nextSource, Nothing, True) ->
          step position direction nextPc nextRb outputs panel nextSource
        (nextPc, nextRb, _, nextSource, Just output, True) ->
          case outputs of
            [] ->
              step position direction nextPc nextRb [output] panel nextSource
            [color] ->
              let
                nextPanel = Map.insert position color panel
                nextDirection = turn direction output
                nextPosition = move nextDirection position
              in
                step nextPosition nextDirection nextPc nextRb [] nextPanel nextSource
  in
    step (0, 0) DUp 0 0 [] Map.empty source

main :: IO ()
main =
  let
    parse = map (fst . fromRight (-1, "") . signed decimal . T.strip) . T.splitOn ","
    buildSource = foldl (flip (uncurry Map.insert)) Map.empty . zip [0..]
  in do
    contents <- T.pack <$> getContents
    putStrLn $ show $ Map.size $ runProgram $ buildSource $ parse contents

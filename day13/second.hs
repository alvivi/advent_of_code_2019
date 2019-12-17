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

import  Debug.Trace

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

findKeyByValue :: Eq b => b -> Map a b -> a
findKeyByValue v map = fst $ head $ filter ((== v) . snd) $ Map.assocs map

play :: Memory -> Int
play =
  let
    joystick screen =
      fromEnum (compare (fst $ findKeyByValue 4 screen) (fst $ findKeyByValue 3 screen)) - 1

    step pc rb outputs screen score source =
      case runInstruction pc rb [joystick screen] source of
        (nextPc, nextRb, _, nextSource, output, False) ->
          score

        (nextPc, nextRb, _, nextSource, Nothing, True) ->
          step nextPc nextRb outputs screen score nextSource

        (nextPc, nextRb, _, nextSource, Just output, True) ->
          case outputs of
            [0, -1] ->
              step nextPc nextRb [] screen output nextSource
            [y, x] ->
              let newScreen = Map.insert (x, y) output screen
              in step nextPc nextRb [] newScreen score nextSource
            _ ->
              step nextPc nextRb (output : outputs) screen score nextSource
  in
    step 0 0 [] Map.empty 0 . Map.insert 0 2

main :: IO ()
main =
  let
    parse = map (fst . fromRight (-1, "") . signed decimal . T.strip) . T.splitOn ","
    buildSource = foldl (flip (uncurry Map.insert)) Map.empty . zip [0..]
  in do
    source <- buildSource <$> parse <$> T.pack <$> getContents
    putStrLn $ show $ play source

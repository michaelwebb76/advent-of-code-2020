{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import Control.Applicative
import Data.Bifunctor
import Data.List (intercalate)
import Data.Map ((!), Map, elems, fromList, keys, lookup, mapWithKey)
import Data.Maybe
import Text.RawString.QQ
import Text.Trifecta
import Prelude hiding (lookup)

-- Tests with workings

part1TestWithWorkings :: IO ()
part1TestWithWorkings =
  testWithWorkings part1Reducer

part2TestWithWorkings :: IO ()
part2TestWithWorkings =
  testWithWorkings part2Reducer

testWithWorkings :: Show a => (Input -> a) -> IO ()
testWithWorkings reducer =
  let parsedInput = parseString parseInput mempty shortInput
   in case parsedInput of
        Success input ->
          putStrLn $
            showInputs
              input
              ++ "\n"
              ++ show
                (reducer input)
        _ ->
          putStrLn "Failed to parse"

-- Actual submittable solution

part1Solution :: IO ()
part1Solution =
  solution longInput part1Reducer

part2Solution :: IO ()
part2Solution =
  solution longInput part2Reducer

part1Reducer :: Input -> Int
part1Reducer input =
  let nextIteratedSeating = part1IterateSeating input
   in if input == nextIteratedSeating
        then length $ filter (== OccupiedSeat) (elems input)
        else part1Reducer nextIteratedSeating

part2Reducer :: Input -> Int
part2Reducer input =
  let nextIteratedSeating = part2IterateSeating input
   in if input == nextIteratedSeating
        then length $ filter (== OccupiedSeat) (elems input)
        else part2Reducer nextIteratedSeating

part1IterateSeating :: Input -> Input
part1IterateSeating inputs =
  mapWithKey (\coords _ -> part1UpdatedSeat inputs coords) inputs

part1UpdatedSeat :: Input -> (Int, Int) -> Seat
part1UpdatedSeat inputs coords =
  let seat = lookup coords inputs
      adjacent = adjacentSeats inputs coords
   in case seat of
        Just Floor ->
          Floor
        Just EmptySeat ->
          if OccupiedSeat `notElem` adjacent
            then OccupiedSeat
            else EmptySeat
        Just OccupiedSeat ->
          if length (filter (== OccupiedSeat) adjacent) >= 4
            then EmptySeat
            else OccupiedSeat
        Nothing ->
          error $ "no seat at " ++ show coords

part2IterateSeating :: Input -> Input
part2IterateSeating inputs =
  mapWithKey (\coords _ -> part2UpdatedSeat inputs coords) inputs

part2UpdatedSeat :: Input -> (Int, Int) -> Seat
part2UpdatedSeat input coords =
  let seat = lookup coords input
      seenSeats =
        mapMaybe
          (firstSeatWithIncrement input coords)
          (filter (/= (0, 0)) [(lx, ly) | lx <- [(-1) .. 1], ly <- [(-1) .. 1]])
   in case seat of
        Just Floor ->
          Floor
        Just EmptySeat ->
          if OccupiedSeat `notElem` seenSeats
            then OccupiedSeat
            else EmptySeat
        Just OccupiedSeat ->
          if length (filter (== OccupiedSeat) seenSeats) >= 5
            then EmptySeat
            else OccupiedSeat
        Nothing ->
          error $ "no seat at " ++ show coords

firstSeatWithIncrement :: Input -> (Int, Int) -> (Int, Int) -> Maybe Seat
firstSeatWithIncrement input coords increment =
  let nextSeatCoords = (\(x, y) -> bimap (x +) (y +) increment) coords
   in case lookup nextSeatCoords input of
        Just Floor ->
          firstSeatWithIncrement input nextSeatCoords increment
        other ->
          other

adjacentSeats :: Input -> (Int, Int) -> [Seat]
adjacentSeats inputs (x, y) =
  let adjacentCoords = [(lx, ly) | lx <- [(x - 1) .. x + 1], ly <- [(y - 1) .. y + 1]]
   in mapMaybe
        ( \(ix, iy) ->
            if (ix, iy) /= (x, y)
              then lookup (ix, iy) inputs
              else Nothing
        )
        adjacentCoords

solution :: (Show a) => String -> (Input -> a) -> IO ()
solution unparsedInput reducer =
  let parsedInput = parseString parseInput mempty unparsedInput
   in print $ reducer <$> parsedInput

showInputs :: Input -> String
showInputs input =
  let maximumRow = maximum (map snd (keys input))
      maximumColumn = maximum (map fst (keys input))
   in intercalate "\n" $
        map
          ( \row ->
              concatMap (\column -> show $ input ! (column, row)) [0 .. maximumColumn -1]
          )
          [0 .. maximumRow -1]

-- Types

data Seat = Floor | EmptySeat | OccupiedSeat deriving (Eq)

instance Show Seat where
  show = \case
    Floor ->
      "."
    EmptySeat ->
      "L"
    OccupiedSeat ->
      "#"

type Input = Map (Int, Int) Seat

-- Parsers

parseInput :: Parser Input
parseInput =
  mapSeats
    <$> sepBy
      ( some $
          choice
            [ Floor <$ char '.',
              EmptySeat <$ char 'L',
              OccupiedSeat <$ char '#'
            ]
      )
      (char '\n')

mapSeats :: [[Seat]] -> Input
mapSeats seats =
  fromList $
    concatMap
      ( \y ->
          map (\x -> ((x, y), (seats !! y) !! x)) [0 .. length (seats !! y) - 1]
      )
      [0 .. (length seats - 1)]

-- Input

shortInput :: String
shortInput =
  [r|L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL|]

longInput :: String
longInput =
  [r|LLLLLLLLLLLL.LLLLLLL.LLLLLLL..LLLL.LLLLL.LLLLL.LLL..LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLL.L.LLL.LLLLL.LLLL..LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.L.LLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL..LLLLL.
L.LLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLL.LL.L.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL
LLLLL..LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LL.LLLL
LLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LL.LLLLL.LLLL.LLLLLLLL.LLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLL
.L...LL.L.L.L....L..L......LL...L..L.L.LLL.....L...L..LLL.L.LL..LLL......L..L.LLLLL.....L..L...
LLLLL..LLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLL.LLLLLLLLLLLLLLLLLLLLL
LLL.LLLLLLLL..LLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLL.L.LLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL
LLLLL.L.LLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLL..LLLLLLL.LLLLLLLLLL.LL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLL.LLLLLLL.LLL.LLLL.LLLLLLL
LLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLL...LLLLLL
LLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLLL.LLLLLL.LLLLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LL.LL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLL..LLLLLLLLLLLLL.LLLLL.L
L.LLLLLLLLLL.LLLLLLLLL.LLLLL.L.LLL.LLLLL.LLLLLLLLLL.LLLLLLLL.LLLL.LLLLL.L.LLLL.LLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LL.LL.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLL.LLLL.LLLLLLL.LLLL.L.LLLLLL.LLLLLLL
L...L.LL..L.L.LL.LL.L.....L..L.L.......L.L.LL...L....L......L.L.L.L.L..L....LL......L......L...
LLLL..LLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL.L.LLLL.LLLL.LLLLL..LLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLL.LL.LL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.
L..LL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LL.L.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL..LLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLL
LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLL.LLLLL.L.LLLLL.LLLLLLL
LL.LL.LLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLL.LLL
...............L.L..LL.....L.L..L...LL.......LL....L......L...L....L...............LLL...L.LLL.
LLLLLLLLLLL..LLLLLL.LLLLLLLL.LLLLL.LLLLL.LLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.L.LLLLL
LL.LL.LLLLLL.LLLLLLLLL.LLLLL.LLLL..LLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLLL.LLL.LLLLLLLLL.LLLLLLL
.LLLL.LLLLLLL.LLLLLLLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLL.LLL.LLLLLLLLLLLL.LLLLL.LLLLLLLLLL
LLLLL.L.LLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLL..LLLLLLLLLLLLLLLL
LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLL.LLLL.LLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLL
LLL.L.LLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL
....L..L..........L......L.LL..L.L..LL.......L.....LL..L..LL...L......L....L..L..L....L.......L
.L.LLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLL.LLLLLL.LLLLLLLLLL.LL.LLLLLLLLLL.L.LLLL.LLLLLLLL.LLLLLLL
LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLLLLLL.LLLLLLL
LLLLL.LLL.LL.LLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLLL.LLLL.L.LLLLLLLLLL..LLLLLLLLLLLL.LLLLLLLL.LLLLLLL
LL.LL.LLLLLL.LLLL.LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.L.LLLLLLLL.LLLLLLLLLLLL.L.LL.LLLLLLLL.LLLLLLL
LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLL..LLLLLLL
.....L.L..LL.LLL.LL.L.L.....L..L.L......L......L.LL...L....LL..L.........L..L.....L.L....L..L..
LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLL..LLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLLLLL.L.LLL.LLLLL.LL.LL.LLLL.L.LLLLL...LLL.LLL.LLLLLLLL.LLLLLLLL.LLLLLLL
LLLLL.L.LLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLL
LLLLL.LLLLL..LLLLLLLLL.L.LLL.LLL.L.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.
LLLLLLLLLLLL.LLLLLLLL..LLLLLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLL.LLLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL
...L..LL.L......L....LLL....L.L.LL..L.L..L.LL..L..L.L......L...L.L..L..LL....L..L..LL...LL.....
L..LL.LLLLLL.LLLLLLLLLL.LL.L.LLLLL.L.LLL.LLLLLL.LLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLL.LLLL..LL.LLLLLLLL.LLLLLLLL.LLLLLLL
LLLLLLLLLLLL.L.LLLLLLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLL
LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL..LLLLLLLLLLLLL.LLLLLLL
LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLL.LLLLLL.LLLL.L.LLL.LL.LLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLL
LLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLL..LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLL.L.LLLL.LLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL
LL..L.L.L.L..LL...LL....L.L..L..LLL..L.LL.L.LLLL..L.L.L..L...L.....L..LL........L...LL..L.L.LL.
LLLLL.LLLL.L.LL.LLLLLL.LLLLLLLLLLLLL.LLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.L.LLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLLL.LLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL
LLLLL.LLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LL.L.LLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL
LLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLL..LLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL
LL.L..L..L.....LL......L.......LLLLL..L.L.LL...L......L.L..L.L.L........L..L.L.L..LLLL....L....
LLL.L.LLLLLLLLLLL.LLLL.LLLLL..LLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.L.LLL.LLLLLLL.LLLLLLL
LLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLL.LLLL..LLL.LLLLLL.LLLLLLLL.LLLL.LL.LLLLLLLLLLLLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLLL..LLLLLLL.L.LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LL.LLLLL.LLLLLLL
LLLLL.L.LLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LL.LLLLL.LLLLLLL
LLLLL.LLLLLLLLLLLLLL.L.LLLLL.L.LLL.LLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLLL.LL.L.LLL.LLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLL.
LLLLL.LLLLLLLLLLLL.LLL.LLL.L.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLL..LLLL.LLLLLLLL..LLLLLL
LL.LL..L......L...L.....LL...L......L..L..LL...LL..L.L.....L.L...L...........LL...L.L.L..L.L.L.
LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LLL.LLLL.LLLLLLL
LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLL.L.LLL..L.LLLLL.LLLLLLLLLLLLL.LLLLLLL
LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLL
LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL..LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL..LLLLLLL.LLLLLLL
LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLL.LL.LLLL
L....L..L..L...LL.L....L..LL.LL..L....L.L.....LL..........L...L...L..L.L.........L....L..L.....
LLLLL.LLLL.LLLLL.LLLLL..LLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.L.LLLLLLLLLL.LLLLLLLLLLL.LLLL
..LLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLL.LLLLLLL
LLL.L.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLL.L.LLLL.LLLLLLLLLLLLLLLL
L.........LL.L..L.L....L.LLLL...L.L.....LLLL.........LL..LL....L...L..L.L...LL..LL...L.L.L..L.L
LLLLL.LLLLLL..LLLLLLLL.LLL.L.LLLLL.LLL.L.LLLLL.L.LL.LLLLLLLL.LLLL.L.LLLLL...LL.LL.LLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLL.L.LLLLLLLLL.LLLLL.LLL.L.LLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLL.LL.L.LLLLLLLL.L.LLLLL
LLLLL.LLLLLL.LLLLLLLLLLLLLL..LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL.LLLL.LLL.LLLLLLL
LLLLLLLLLLL..LLLLLLLLL.LLLLL.LLLLLLLLLLL.L.LLL.LLLL.LLLLLLLL.LLLL.LLLL.LL.LLL.LLLLLLLLLLLLLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLLL..LLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLL.LL.LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLL
.LLLL.LLLLLL.LLLLLLLLL.LLLLL.L.LLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LL.L.LLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.L.L..LLLLLLLLLLLLLLLL|]

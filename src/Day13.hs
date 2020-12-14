{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Day13 where

import Data.List
import Data.Maybe
import Text.RawString.QQ
import Text.Trifecta

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
   in case (\input -> show input ++ "\n" ++ show (reducer <$> parsedInput)) <$> parsedInput of
        Success string ->
          putStrLn string
        _ ->
          putStrLn "FAIL"

-- Actual submittable solution

part1Solution :: IO ()
part1Solution =
  solution longInput part1Reducer

part2Solution :: IO ()
part2Solution =
  solution longInput part2Reducer

part1Reducer :: Input -> Solution
part1Reducer (Input departureTime maybeBusIds) =
  let actualBusIds = catMaybes maybeBusIds
      timesToCheck = [departureTime ..]
      busIdDepartingAtTime time = find (\busId -> time `mod` busId == 0) actualBusIds
   in case find (isJust . busIdDepartingAtTime) timesToCheck of
        Just time ->
          case busIdDepartingAtTime time of
            Just busId ->
              Solution busId time (time - departureTime)
            Nothing ->
              error "no buses arrive before that time"
        Nothing ->
          error "no buses arrive before that time"

part2Reducer :: Input -> DepartureTime
part2Reducer (Input _ maybeBusIds) =
  let idsIndexes = idsAndIndexes maybeBusIds
   in lastGasp 0 [1] idsIndexes

lastGasp :: Int -> [Int] -> [(Int, Int)] -> Int
lastGasp lowestMatch _ [] = lowestMatch
lastGasp lowestMatch multiplicands ((busId, index) : xs) =
  let numbersToSearch = enumFromThen lowestMatch (lowestMatch + product multiplicands)
   in lastGasp (fromJust $ find (\int -> (int + index) `mod` busId == 0) numbersToSearch) (multiplicands ++ [busId]) xs

idsAndIndexes :: [Maybe BusId] -> [(Int, Int)]
idsAndIndexes maybeBusIds =
  (catMaybes . snd) $
    mapAccumL
      ( \index maybeBusId ->
          ( index + 1,
            (,index) <$> maybeBusId
          )
      )
      0
      maybeBusIds

solution :: (Show a) => String -> (Input -> a) -> IO ()
solution unparsedInput reducer =
  let parsedInput = parseString parseInput mempty unparsedInput
   in print $ reducer <$> parsedInput

-- Types

type DepartureTime = Int

type WaitTime = Int

type BusId = Int

data Input = Input DepartureTime [Maybe BusId] deriving (Eq, Show)

data Solution = Solution BusId DepartureTime WaitTime deriving (Eq)

instance Show Solution where
  show (Solution busId departureTime waitTime) =
    "Catch bus "
      ++ show busId
      ++ " at "
      ++ show departureTime
      ++ ", you'll wait "
      ++ show waitTime

-- Parsers

parseInput :: Parser Input
parseInput =
  Input
    <$> (fromIntegral <$> decimal <* char '\n')
    <*> sepBy
      ( choice
          [ Nothing <$ char 'x',
            Just . fromIntegral <$> decimal
          ]
      )
      (char ',')

-- Input

shortInput :: String
shortInput =
  [r|939
7,13,x,x,59,x,31,19|]

longInput :: String
longInput =
  [r|1008832
23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,449,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,19,x,x,x,x,x,x,x,x,x,29,x,991,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,17|]

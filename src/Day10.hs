{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Day10 where

import Control.Applicative
import Data.Either
import Data.List
import Data.Maybe
import Text.RawString.QQ
import Text.Trifecta

-- Tests with workings

part1TestWithWorkings :: IO ()
part1TestWithWorkings =
  testWithWorkings part1Matcher part1Reducer

part2TestWithWorkings :: IO ()
part2TestWithWorkings =
  testWithWorkings part2Matcher part2Reducer

testWithWorkings :: Show a => (Input -> Bool) -> ([Input] -> a) -> IO ()
testWithWorkings matcher reducer =
  let parsedInput = parseString parseInput mempty shortInput
      filteredParsedInput = filter matcher <$> parsedInput
   in case ( \inputs ->
               unlines
                 ( map
                     ( \input ->
                         if matcher input
                           then show input ++ " (match)"
                           else show input ++ " (no match)"
                     )
                     (sort inputs)
                 )
                 ++ show (reducer <$> filteredParsedInput)
           )
        <$> parsedInput of
        Success string ->
          putStrLn string
        _ ->
          putStrLn "FAIL"

-- Actual submittable solution

part1Solution :: IO ()
part1Solution =
  solution longInput part1Matcher part1Reducer

part2Solution :: IO ()
part2Solution =
  solution longInput part2Matcher part2Reducer

part1Reducer :: [Input] -> Int
part1Reducer inputs =
  let differences = (listDiffs . sort) inputs
   in (length . filter (== 1)) differences * ((length . filter (== 3)) differences + 1)

listDiffs :: [Input] -> [Input]
listDiffs = snd . mapAccumL (\a b -> (b, b - a)) 0

part2Reducer :: [Input] -> Int
part2Reducer =
  snd . last . pathwayCountFromInputs . ([0] ++) . sort

pathwayCountFromInputs :: [Input] -> [(Input, Int)]
pathwayCountFromInputs =
  foldr
    ( \input inputPathCounts ->
        let plusOnePathways = find (\(i, _) -> i == input + 1) inputPathCounts
            plusTwoPathways = find (\(i, _) -> i == input + 2) inputPathCounts
            plusThreePathways = find (\(i, _) -> i == input + 3) inputPathCounts
            plusPathways = catMaybes [plusOnePathways, plusTwoPathways, plusThreePathways]
            totalPathways = sum $ map snd plusPathways
         in inputPathCounts ++ [(input, maximum [1, totalPathways])]
    )
    []

solution :: (Show a) => String -> (Input -> Bool) -> ([Input] -> a) -> IO ()
solution unparsedInput matcher reducer =
  let parsedInput = parseString parseInput mempty unparsedInput
      filteredParsedInput = filter matcher <$> parsedInput
   in print $ reducer <$> filteredParsedInput

-- Types

type Input = Int

-- Parsers

parseInput :: Parser [Input]
parseInput =
  sepBy
    (read <$> some digit)
    (char '\n')

-- Matchers

part1Matcher :: Input -> Bool
part1Matcher _ =
  True

part2Matcher :: Input -> Bool
part2Matcher _ =
  True

-- Input

shortInput :: String
shortInput =
  [r|16
10
15
5
1
11
7
19
6
12
4|]

mediumInput :: String
mediumInput =
  [r|28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3|]

longInput :: String
longInput =
  [r|59
134
159
125
95
92
169
43
154
46
110
79
117
151
141
56
87
10
65
170
89
32
40
118
36
94
124
173
164
166
113
67
76
102
107
52
144
119
2
72
86
73
66
13
15
38
47
109
103
128
165
148
116
146
18
135
68
83
133
171
145
48
31
106
161
6
21
22
77
172
28
78
96
55
132
39
100
108
33
23
54
157
80
153
9
62
26
147
1
27
131
88
138
93
14
123
122
158
152
71
49
101
37
99
160
53
3|]

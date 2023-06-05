module Hadvent.Day2 where

import Data.Maybe

import qualified Hadvent.Utils as U

sample :: String
sample =
  "5 1 9 5\n\
 \7 5 3\n\
 \2 4 6 8"

minAndMax :: (Num a, Ord a) => [a] -> Maybe (a, a)
minAndMax (y:ys) = Just $ minAndMax' ys y y
  where
    minAndMax' :: (Num a, Ord a) => [a] -> a -> a -> (a, a)
    minAndMax' [] mn mx = (mn, mx)
    minAndMax' (x:rest) mn mx
      | x < mn = minAndMax' rest x mx
      | mx < x = minAndMax' rest mn x
      | otherwise = minAndMax' rest mn mx
minAndMax _ = Nothing

divEvenly :: [Int] -> Int
divEvenly [] = undefined
divEvenly (x:rest) =
  case findDiv x rest of
    Just n -> n
    Nothing -> divEvenly rest
  where
    findDiv :: Int -> [Int] -> Maybe Int
    findDiv n [] = Nothing
    findDiv n (x:rest)
      | n `mod` x == 0 = Just $ n `div` x
      | x `mod` n == 0 = Just $ x `div` n
      | otherwise = findDiv n rest

runSolution :: ([Int] -> Int) -> String -> Int
runSolution f = sum . map f . map (map U.readInt) . map words . lines

solveFirst :: String -> Int
solveFirst = runSolution ((uncurry $ flip $ (-)) . fromJust . minAndMax)

solveSecond :: String -> Int
solveSecond = runSolution divEvenly

tests :: IO ()
tests = do
  putStrLn "Testing MinAndMax"
  U.runTests
    [ ((1, 5), fromJust $ minAndMax [3, 5, 2, 1])
    , ((0, 0), fromJust $ minAndMax [0, 0, 0])
    ]
  putStrLn "Testing solveFirst"
  U.runTests [(18, solveFirst sample), (50376, solveFirst input)]
  putStrLn "Testing solveSecond"
  U.runTests [(267, solveSecond input)]
  putStrLn "Testing all"
  U.run
    [ U.t ((1, 5), fromJust $ minAndMax [3, 5, 2, 1])
    , U.t ((0, 0), fromJust $ minAndMax [0, 0, 0])
    , U.t (267, solveSecond input)
    ]

input :: String
input =
  "6046 6349 208 276 4643 1085 1539 4986 7006 5374 252 4751 226 6757 7495 2923\n\
 \1432 1538 1761 1658 104 826 806 109 939 886 1497 280 1412 127 1651 156\n\
 \244 1048 133 232 226 1072 883 1045 1130 252 1038 1022 471 70 1222 957\n\
 \87 172 93 73 67 192 249 239 155 23 189 106 55 174 181 116\n\
 \5871 204 6466 6437 5716 232 1513 7079 6140 268 350 6264 6420 3904 272 5565\n\
 \1093 838 90 1447 1224 744 1551 59 328 1575 1544 1360 71 1583 75 370\n\
 \213 166 7601 6261 247 210 4809 6201 6690 6816 7776 2522 5618 580 2236 3598\n\
 \92 168 96 132 196 157 116 94 253 128 60 167 192 156 76 148\n\
 \187 111 141 143 45 132 140 402 134 227 342 276 449 148 170 348\n\
 \1894 1298 1531 1354 1801 974 85 93 1712 130 1705 110 314 107 449 350\n\
 \1662 1529 784 1704 1187 83 422 146 147 1869 1941 110 525 1293 158 1752\n\
 \162 1135 3278 1149 3546 3686 182 149 119 1755 3656 2126 244 3347 157 865\n\
 \2049 6396 4111 6702 251 669 1491 245 210 4314 6265 694 5131 228 6195 6090\n\
 \458 448 324 235 69 79 94 78 515 68 380 64 440 508 503 452\n\
 \198 216 5700 4212 2370 143 5140 190 4934 539 5054 3707 6121 5211 549 2790\n\
 \3021 3407 218 1043 449 214 1594 3244 3097 286 114 223 1214 3102 257 3345"

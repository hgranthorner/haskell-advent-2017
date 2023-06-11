module Hadvent.Day3 where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Ix
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Hadvent.Utils

data Cell = Cell
  { value :: Int
  , x :: Int
  , y :: Int
  } deriving (Show)

initialCell :: Cell
initialCell = Cell {value = 1, x = 0, y = 0}

data Dir
  = U
  | D
  | L
  | R
  deriving (Bounded, Show)

data Step = Step
  { currentValue :: Int
  , prevStep :: Int
  } deriving (Show)

type CornerValue = Int

corners :: [CornerValue]
corners = scanl (+) 1 stepValues
  where
    stepValues = [1 ..] >>= replicate 2

addRotation :: CornerValue -> CornerValue -> Dir -> State Cell [Cell]
addRotation start end dir = do
  previousCell <- get
  let (val, lastCell) =
        runState (mapM genNextCell $ range (succ start, end)) previousCell
  put lastCell
  return val
  where
    genNextCell _ = do
      pc <- get
      let nc = nextCell pc dir
      put nc
      return nc

grid :: [Cell]
grid =
  concat
    $ (:) [initialCell]
    $ f [] initialCell
    $ map (\(s, e, d) -> addRotation s e d)
    $ zip3 corners (tail corners) dirs
  where
    f :: [[Cell]] -> Cell -> [State Cell [Cell]] -> [[Cell]]
    f result prevCell (s:rest) =
      let (stateResult, finalState) = runState s prevCell
       in stateResult : f result finalState rest
    f result prevCell [] = result

nextCell :: Cell -> Dir -> Cell
nextCell c U = c {value = succ $ value c, y = succ $ y c}
nextCell c L = c {value = succ $ value c, x = pred $ x c}
nextCell c D = c {value = succ $ value c, y = pred $ y c}
nextCell c R = c {value = succ $ value c, x = succ $ x c}

dirs :: [Dir]
dirs = cycle [R, U, L, D]

solveFirst :: Int -> Int
solveFirst num =
  let grid' = take num grid
      Cell {x = x, y = y} = last grid'
   in abs x + abs y

type Pos = (Int, Int)

type CellMap = M.Map Pos Cell

gridMap :: Int -> CellMap
gridMap num = buildMap initialMap initialCell $ take num $ zip3 corners (tail corners) dirs
  where
    buildMap :: CellMap -> Cell -> [(CornerValue, CornerValue, Dir)] -> CellMap
    buildMap cm c [] = cm
    buildMap cm c ((start, end, dir):xs) =
      let (cm', lastCell) =
            addRotationMap cm c start end dir
       in if value lastCell > num
            then cm'
            else buildMap cm' lastCell xs
    initialMap = M.singleton (0, 0) initialCell

sumOfSurrounding :: CellMap -> Pos -> Int
sumOfSurrounding cm (x, y) = sum values
  where
    xs = range (x - 1, x + 1)
    ys = range (y - 1, y + 1)
    values :: [Int]
    values =
      map (fromMaybe 0 . fmap value . flip M.lookup cm) $ liftM2 (,) xs ys

addRotationMap ::
     CellMap -> Cell -> CornerValue -> CornerValue -> Dir -> (CellMap, Cell)
addRotationMap currentMap previousCell start end dir =
        genNextCell (range (succ start, end)) currentMap previousCell
  where
    genNextCell :: [a] -> CellMap -> Cell -> (CellMap, Cell)
    genNextCell [] cm previousCell = (cm, previousCell)
    genNextCell (_:xs) cm previousCell = genNextCell xs nextMap nextCell''
      where
        nextCell' = nextCell previousCell dir
        nextCell'' =
          nextCell' {value = sumOfSurrounding cm (x nextCell', y nextCell')}
        nextMap = M.insert (x nextCell', y nextCell') nextCell'' cm

solveSecond :: Int -> Int
solveSecond num = minimum $ filter (> num) $ map value $ M.elems $ gridMap num

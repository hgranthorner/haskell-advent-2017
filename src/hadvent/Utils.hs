{-# LANGUAGE ExistentialQuantification #-}

module Hadvent.Utils
  ( readInt
  , run
  , runTests
  , t
  , tests
  ) where

import Control.Exception
import Control.Monad

data Test =
  forall a. Eq a =>
            MkTest (a, a)

t :: forall a. Eq a
  => (a, a)
  -> Test
t = MkTest

tests ::
     forall a. Eq a
  => [(a, a)]
  -> [Test]
tests = map MkTest

run :: [Test] -> IO ()
run xs = forM_ (zip [(1 :: Int) ..] xs) f
  where
    f (i, (MkTest (ex, act))) =
      putStrLn
        $ assert (ex == act)
        $ "Test " ++ (show i) ++ " completed successfully!"

runTests :: Eq a => [(a, a)] -> IO ()
runTests xs = forM_ (zip [(1 :: Int) ..] xs) f
  where
    f (i, (ex, act)) =
      putStrLn
        $ assert (ex == act)
        $ "Test " ++ (show i) ++ " completed successfully!"

readInt :: String -> Int
readInt = read



{-# OPTIONS -W #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import System.Random

import RTree

data Ship = Saucer 
  { posx :: Int
  , posy :: Int
  , r :: Int } deriving Show

instance Spatial Ship where
  data Interval Ship = SI (Maybe ((Int,Int),(Int,Int))) deriving Show
  type IntervalUnit Ship = Int
  emptyInterval = SI Nothing
  cover (SI Nothing) i2 = i2
  cover i1 (SI Nothing) = i1
  cover (SI (Just ((xa1, ya1), (xa2, ya2)))) (SI (Just ((xb1, yb1), (xb2, yb2)))) =
    let i1 = (min xa1 xb1, max ya1 yb1)
        i2 = (min xa2 xb2, max ya2 yb2)
    in SI (Just (i1,i2))
  intersect (SI Nothing) _ = False
  intersect _ (SI Nothing) = False
  intersect (SI (Just ((xa1, ya1), (xa2, ya2)))) (SI (Just ((xb1, yb1), (xb2, yb2)))) = 
    not ((xa2 < xb1) || (xb2 < xa1) || (ya2 < yb1) || (yb2 < ya1))
  size (SI Nothing) = -1
  size (SI (Just ((x1,y1),(x2,y2)))) = (x2 - x1) * (y2 - y1)
  bounds ship = SI . Just $ ((posx ship - r ship
                             ,posx ship + r ship)
                            ,(posy ship - r ship
                             ,posy ship + r ship))

instance Container TVar where
  type CMonad TVar = STM
  newC = newTVar
  readC = readTVar
  writeC = writeTVar

mkShip :: Int -> Ship
mkShip i =
  let g1 = mkStdGen i
      (bigx, g2) = next g1
      (bigy, g3) = next g2
      (bigr, _) = next g3
  in Saucer (mod bigx 100) (mod bigy 100) (mod bigr 15 + 5)

shipsTest :: IO ()
shipsTest = do
  putStrLn "Go"
  tree :: RTree TVar Ship <- atomically $ empty defaultConfig
  (n :: Int) <- readLn
  forM_ [1..n] (\_ -> do
    i <- readLn
    putStrLn . unwords $ [ "Inserting", show i, "into tree" ]
    atomically $ insert tree $ mkShip i)
  atomically (readTree tree) >>= printTree

main :: IO ()
main = shipsTest




{-# OPTIONS -W #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import System.Random

import RTree

data Ship = Saucer 
  { posx :: Int
  , posy :: Int
  , r :: Int } deriving Show

type ShipStore = TVar (Map Int Ship)

data ShipHandle = ShipHandle
  { tStore :: ShipStore
  , sid :: Int
  }

instance Show ShipHandle where
  show handle = unwords ["ShipHandle", show (sid handle)]

getShip :: ShipHandle -> STM (Maybe Ship)
getShip handle = do
  store <- readTVar $ tStore handle
  return $ M.lookup (sid handle) store

instance Spatial ShipHandle where

  data Interval ShipHandle = SI (Maybe ((Int,Int),(Int,Int))) deriving Show
  type IntervalUnit ShipHandle = Int
  type SMonad ShipHandle = STM
  
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

  bounds handle = do
    mship <- getShip handle
    return $ case mship of
      Nothing -> SI Nothing
      Just ship -> SI . Just $ ((posx ship - r ship
                                         ,posx ship + r ship)
                                        ,(posy ship - r ship
                                         ,posy ship + r ship))

instance Container TVar where
  type CMonad TVar = STM
  newC = newTVar
  readC = readTVar
  writeC = writeTVar

genShip :: ShipStore -> Int -> Int -> STM ShipHandle
genShip tStore sid seed =
  let g1 = mkStdGen seed
      (bigx, g2) = next g1
      (bigy, g3) = next g2
      (bigr, _) = next g3
      ship = Saucer (mod bigx 100) (mod bigy 100) (mod bigr 15 + 5)
  in do
    store <- readTVar tStore
    writeTVar tStore $ M.insert sid ship store
    return $ ShipHandle tStore sid

shipsTest :: IO ()
shipsTest = do
  putStrLn "Go"
  shipStore <- atomically $ newTVar M.empty
  tree :: RTree TVar ShipHandle <- atomically $ empty defaultConfig
  (n :: Int) <- readLn
  forM_ [1..n] (\sid -> do
    seed <- readLn
    putStrLn . unwords $ [ "Inserting", show sid, show seed, "into tree" ]
    atomically $ do
      handle <- genShip shipStore sid seed
      insert tree handle)
  atomically (readTree tree) >>= printTree

main :: IO ()
main = shipsTest


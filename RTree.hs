
{-# OPTIONS -W #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module RTree (Container(..), Spatial(..), RTreeConfig, defaultConfig, RTree, empty, readTree, printTree, search, insert) where

import Control.Applicative hiding (empty)
import Control.Monad.Identity
import qualified Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as V

class (Functor (CMonad c), Monad (CMonad c)) => Container c where
  type CMonad c :: * -> *
  newC :: a -> (CMonad c) (c a)
  readC :: (c a) -> (CMonad c) a
  writeC :: (c a) -> a -> (CMonad c) ()

class (Ord (IntervalUnit a), Num (IntervalUnit a)) => Spatial a where
  data Interval a :: *
  type IntervalUnit a :: *
  emptyInterval :: Interval a
  cover :: Interval a -> Interval a -> Interval a
  intersect :: Interval a -> Interval a -> Bool
  size :: Interval a -> IntervalUnit a
  bounds :: a -> Interval a

data RTreeConfig = RTreeConfig
  { maxElems :: Int
  , minElems :: Int
  }

defaultConfig :: RTreeConfig
defaultConfig = RTreeConfig 6 2

data RTree c a = RTree
  { config :: RTreeConfig
  , root :: c (Interval a, RNode c a)
  }

type VBounded a b = Vector (Interval a, b)
type CVBounded m a b = m (VBounded a b)

data RNode c a 
  = RInternal (CVBounded c a (RNode c a))
  | RLeaf (CVBounded c a a)

readTree :: (Container c) => RTree c a -> (CMonad c) (RTree Identity a)
readTree tree = 
  let readNode :: (Container c) => RNode c a -> (CMonad c) (RNode Identity a)
      readNode node = case node of
        RLeaf cChilds -> do
          childs <- readC cChilds
          return $ RLeaf $ Identity childs
        RInternal cChilds -> do
          childs <- readC cChilds
          childs' <- V.mapM (\(i,n) -> (i,) <$> readNode n) childs
          return $ RInternal $ Identity childs'
  in do
    (i,rootNode) <- readC (root tree)
    rootNode' <- readNode rootNode
    return $ RTree (config tree) (Identity (i,rootNode'))

printTree :: (Show a, Show (Interval a)) => RTree Identity a -> IO ()
printTree tree = 

  let padStep :: String
      padStep = "->"

      printNode :: (Show a, Show (Interval a)) => String -> (Interval a, RNode Identity a) -> IO ()
      printNode pad (region,node) = case node of

        RLeaf (Identity childs) -> do
          putStrLn $ unwords [pad, "Leaf", show region, show childs]

        RInternal (Identity childs) -> do
          putStrLn $ unwords $ [pad, "Internal", show region] ++ (map (const "Child") $ V.toList childs)
          V.forM_ childs $ printNode (padStep ++ pad)

  in do
    putStrLn "Root"
    printNode padStep $ runIdentity (root tree)

empty :: (Container c, Spatial a) => RTreeConfig -> (CMonad c) (RTree c a)
empty cfg = do
  leaf <- RLeaf <$> newC V.empty
  RTree cfg <$> newC (emptyInterval, leaf)

data RInsert c a = RInsert (Interval a) | RSplit (Interval a, RNode c a) (Interval a, RNode c a)

nodeRegion :: Spatial a => VBounded a b -> Interval a
nodeRegion v = V.foldr (cover . fst) emptyInterval v

searchNode :: (Spatial a, Container c) => Interval a -> RNode c a -> (CMonad c) (VBounded a a)
searchNode query node = case node of
  RLeaf cChilds -> do
    childs <- readC cChilds
    return $ V.filter (intersect query . fst) childs
  RInternal cChilds -> do
    childs <- readC cChilds
    let matches = V.filter (intersect query . fst) childs
    results <- V.mapM (searchNode query . snd) matches
    return $ V.foldr (V.++) V.empty results

search :: (Spatial a, Container c) => RTree c a -> Interval a -> (CMonad c) (VBounded a a)
search tree q = (snd <$> readC (root tree)) >>= searchNode q

split :: (Spatial a) => RTreeConfig -> VBounded a b -> (VBounded a b, VBounded a b)
split cfg v = 
  let nodeMin = minElems cfg
      l = V.length v
      is = [ (a,b) | a <- [0..(l-1)], b <- [0..(a-1)] ]
      worsePair (a,b) (c,d) =
        let ab = size $ cover (fst $ v V.! a) (fst $ v V.! b)
            cd = size $ cover (fst $ v V.! c) (fst $ v V.! d)
        in compare ab cd
      (worst1, worst2) = L.maximumBy worsePair is
      vWorst1 = v V.! worst1
      vWorst2 = v V.! worst2
      remaining = [ i | i <- [0..(l-1)], not $ elem i [worst1, worst2] ]
      (group1, group2) = L.partition (\i -> worsePair (i,worst1) (i,worst2) == LT) remaining
      vGroup1 = V.ifilter (\i _ -> elem i group1) v
      vGroup2 = V.ifilter (\i _ -> elem i group2) v
      (length1, length2) = (V.length vGroup1, V.length vGroup2)
  in 
    if length1 + 1 < nodeMin then
        let (vGroup2A, vGroup2B) = V.splitAt (nodeMin - (length1 + 1)) vGroup2
        in (V.cons vWorst1 (vGroup1 V.++ vGroup2A), V.cons vWorst2 vGroup2B)
    else if length2 + 1 < nodeMin then
        let (vGroup1A, vGroup1B) = V.splitAt (nodeMin - (length2 + 1)) vGroup1
        in (V.cons vWorst1 vGroup1A, V.cons vWorst2 (vGroup1B V.++ vGroup2))
    else (V.cons vWorst1 vGroup1, V.cons vWorst2 vGroup2)

insertNode :: (Container c, Spatial a) => RTreeConfig -> RNode c a -> a -> (CMonad c) (RInsert c a)
insertNode cfg node x = case node of

  RLeaf tChilds -> do
    childs <- readC tChilds
    let newChilds = V.cons (bounds x, x) childs
    if V.length newChilds <= maxElems cfg
    then do
      writeC tChilds newChilds
      return $ RInsert $ nodeRegion newChilds
    else 
      let (group1, group2) = split cfg newChilds
      in do
        split1 <- RLeaf <$> newC group1
        split2 <- RLeaf <$> newC group2
        return $ RSplit (nodeRegion group1, split1) (nodeRegion group2, split2)

  RInternal tChilds -> do
    childs <- readC tChilds
    let predictSizeDelta i = size (cover i (bounds x)) - size i
    let bestChildIndex = V.minIndex . fmap (predictSizeDelta . fst) $ childs
    (_,bestChild) <- V.indexM childs bestChildIndex
    insertResult <- insertNode cfg bestChild x
    case insertResult of
      RInsert i ->
        let newChilds = childs V.// [(bestChildIndex, (i, bestChild))]
        in do
          writeC tChilds newChilds
          return $ RInsert $ nodeRegion newChilds
      RSplit iNode1 iNode2 -> 
        let remainder = V.ifilter (\i _ -> i /= bestChildIndex) childs
            newChilds = V.cons iNode1 $ V.cons iNode2 remainder
        in if V.length newChilds <= maxElems cfg
        then do
          writeC tChilds newChilds
          return $ RInsert $ nodeRegion newChilds
        else
          let (group1, group2) = split cfg newChilds
          in do
            split1 <- RInternal <$> newC group1
            split2 <- RInternal <$> newC group2
            return $ RSplit (nodeRegion group1, split1) (nodeRegion group2, split2)
          
insert :: (Container c, Spatial a) => RTree c a -> a -> (CMonad c) ()
insert tree x = do
    (_,rootNode) <- readC (root tree)
    insertResult <- insertNode (config tree) rootNode x
    case insertResult of
      RInsert i -> writeC (root tree) (i,rootNode)
      RSplit (i1,n1) (i2,n2) ->
        let childs = V.fromList [(i1,n1),(i2,n2)]
            newRootInterval = cover i1 i2
        in do
          newRootNode <- RInternal <$> newC childs
          writeC (root tree) (newRootInterval, newRootNode)



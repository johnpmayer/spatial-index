
{-# OPTIONS -W #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, DataKinds, GADTs, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module RTree (Container(..), Spatial(..), RTreeConfig, defaultConfig, RTree, empty, leafList, readTree, printTree, search, insert) where

import Control.Applicative hiding (empty)
import Control.Monad.Identity
import qualified Data.List as L
import Data.Type.Natural
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (Bounded)

class (Functor (CMonad c), Monad (CMonad c)) => Container c where
  type CMonad c :: * -> *
  newC :: a -> (CMonad c) (c a)
  readC :: (c a) -> (CMonad c) a
  writeC :: (c a) -> a -> (CMonad c) ()

class (Ord (IntervalUnit a), Num (IntervalUnit a), Monad (SMonad a)) => Spatial a where
  data Interval a :: *
  type IntervalUnit a :: *
  type SMonad a :: * -> *
  emptyInterval :: Interval a
  cover :: Interval a -> Interval a -> Interval a
  intersect :: Interval a -> Interval a -> Bool
  size :: Interval a -> IntervalUnit a
  bounds :: a -> SMonad a (Interval a)

data RTreeConfig = RTreeConfig
  { maxElems :: Int
  , minElems :: Int
  }

defaultConfig :: RTreeConfig
defaultConfig = RTreeConfig 6 2

type Bounded a b = (Interval a, b)
type VBounded a b = Vector (Bounded a b)
type CVBounded c a b = c (VBounded a b)

data RNode :: (* -> *) -> * -> Nat -> * where
  RInternal :: (CVBounded c a (RNode c a n)) -> RNode c a (S n)
  RLeaf :: (CVBounded c a a) -> RNode c a Z

data AnyNode c a = forall h. AnyNode (RNode c a h)

data RTree c a = RTree
  { config :: RTreeConfig
  , root :: c (Interval a, AnyNode c a)
  }

{-
-- Collapse from the bottom
foldNode :: (Container c) => (b -> VBounded a b -> (CMonad c) b) -> (b -> VBounded a a -> (CMonad c) b) -> b -> RNode c a -> (CMonad c) b
foldNode foldInternal foldLeaf initial node = case node of
  RInternal cChilds -> do
    childs <- readC cChilds
    bChilds <- V.mapM (\(i,x) -> (i,) <$> foldNode foldInternal foldLeaf initial x) childs
    foldInternal initial bChilds
  RLeaf cLeaves -> do
    childs <- readC cLeaves 
    foldLeaf initial childs
    -}

-- DFS Walk 
walkNode :: (Container c) => (b -> Bounded a a -> (CMonad c) b) -> b -> RNode c a h -> (CMonad c) b
walkNode walkLeaf initial node = case node of
  RInternal cChilds -> do
    childs <- readC cChilds
    V.foldM (\y -> \(_i,x) -> walkNode walkLeaf y x) initial childs
  RLeaf cLeaves -> do
    leaves <- readC cLeaves
    V.foldM walkLeaf initial leaves

leafList :: (Container c) => RTree c a -> (CMonad c) [Bounded a a]
leafList tree = do
  (AnyNode node) <- snd <$> readC (root tree)
  walkNode accum [] node
  where
    accum acc (i,a) = return $ (i,a) : acc

readTree :: (Container c) => RTree c a -> (CMonad c) (RTree Identity a)
readTree tree = 
  let readNode :: (Container c) => RNode c a h -> (CMonad c) (RNode Identity a h)
      readNode node = case node of
        RLeaf cLeaves -> do
          childs <- readC cLeaves
          return $ RLeaf $ Identity childs
        RInternal cChilds -> do
          leaves <- readC cChilds
          RInternal . Identity <$> V.mapM (\(i,n) -> (i,) <$> readNode n) leaves
  in do
    (i,(AnyNode rootNode)) <- readC (root tree)
    rootNode' <- readNode rootNode
    return $ RTree (config tree) (Identity (i, AnyNode rootNode'))

printTree :: (Show a, Show (Interval a)) => RTree Identity a -> IO ()
printTree tree = 

  let padStep :: String
      padStep = "->"

      printNode :: (Show a, Show (Interval a)) => String -> (Interval a, RNode Identity a h) -> IO ()
      printNode pad (region,node) = case node of

        RLeaf (Identity childs) -> do
          putStrLn $ unwords [pad, "Leaf", show region, show childs]
          V.forM_ childs $ \child -> putStrLn $ unwords [padStep ++ pad, show child]

        RInternal (Identity childs) -> do
          putStrLn $ unwords $ [pad, "Internal", show region] ++ (map (const "Child") $ V.toList childs)
          V.forM_ childs $ printNode (padStep ++ pad)

  in do
    putStrLn "Root"
    case runIdentity $ root tree of
      (region, AnyNode node) -> do
        printNode "" (region, node)

empty :: (Container c, Spatial a) => RTreeConfig -> (CMonad c) (RTree c a)
empty cfg = do
  leaf <- AnyNode . RLeaf <$> newC V.empty
  RTree cfg <$> newC (emptyInterval, leaf)

nodeRegion :: Spatial a => VBounded a b -> Interval a
nodeRegion v = V.foldr (cover . fst) emptyInterval v

searchNode :: (Spatial a, Container c) => Interval a -> RNode c a h -> (CMonad c) (VBounded a a)
searchNode query node = case node of
  RLeaf cLeaves -> do
    childs <- readC cLeaves
    return $ V.filter (intersect query . fst) childs
  RInternal cChilds -> do
    childs <- readC cChilds
    let matches = V.filter (intersect query . fst) childs
    results <- V.mapM (searchNode query . snd) matches
    -- Didn't think too much about the complexity here, reconsider a fold and return a list
    return $ V.foldr (V.++) V.empty results

search :: (Spatial a, Container c) => RTree c a -> Interval a -> (CMonad c) (VBounded a a)
search tree q = do
  anyNode <- snd <$> readC (root tree)
  case anyNode of
    AnyNode node -> searchNode q node

pairwise :: (Container c) => 
  (Interval a ->  Interval a -> Bool) -> (a -> a -> b) -> [b] ->
  RNode c a h -> RNode c a h -> (CMonad c) [b]
pairwise test comb acc node1 node2 = case (node1, node2) of
  (RLeaf cLeaves1, RLeaf cLeaves2) -> do
    leaves1 <- readC cLeaves1
    leaves2 <- readC cLeaves2
    V.foldM (\acc1 (i1,leaf1) -> V.foldM (\acc2 (i2,leaf2) -> return $ if test i1 i2 then comb leaf1 leaf2 : acc2 else acc2) acc1 leaves2) acc leaves1
  (RInternal cChilds1, RInternal cChilds2) -> do
    childs1 <- readC cChilds1
    childs2 <- readC cChilds2
    V.foldM (\acc1 (i1,child1) -> V.foldM (\acc2 (i2,child2) -> if test i1 i2 then pairwise test comb acc2 child1 child2 else return acc2) acc1 childs2) acc childs1

collisions :: (Spatial a, Container c) => RTree c a -> (CMonad c) [(a,a)]
collisions tree = do
  anyNode <- snd <$> readC (root tree)
  case anyNode of
    AnyNode node -> pairwise intersect (,) [] node node

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

data RInsert c a h = RInsert (Interval a) | RSplit (Interval a, RNode c a h) (Interval a, RNode c a h)

insertNode :: (m ~ CMonad c, m ~ SMonad a, Container c, Spatial a) => RTreeConfig -> RNode c a h -> a -> m (RInsert c a h)
insertNode cfg node x = case node of

  RLeaf tChilds -> do
    childs <- readC tChilds
    b <- bounds x
    let newChilds = V.cons (b, x) childs
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
    b <- bounds x
    let predictSizeDelta i = size (cover i b) - size i
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
          
insert :: (CMonad c ~ m, SMonad a ~ m, Container c, Spatial a) => RTree c a -> a -> m ()
insert tree x = do
    (_,anyNode) <- readC (root tree)
    case anyNode of
      AnyNode rootNode -> do
        insertResult <- insertNode (config tree) rootNode x
        case insertResult of
          RInsert i -> writeC (root tree) (i, AnyNode rootNode)
          RSplit (i1,n1) (i2,n2) ->
            let childs = V.fromList [(i1,n1),(i2,n2)]
                newRootInterval = cover i1 i2
            in do
              newRootNode <- RInternal <$> newC childs
              writeC (root tree) (newRootInterval, AnyNode newRootNode)


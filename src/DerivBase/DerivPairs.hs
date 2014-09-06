{-------------------------------------------------------------------------------

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module DerivBase.DerivPairs where

import Prelude hiding (succ)
import Data.List
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Maybe
import Data.Char
import qualified Data.ByteString.UTF8 as B
import qualified Data.Map as M
import Control.Monad
import System.Environment
import DerivBase.LemmaClusters
import DerivBase.DerivFamilies

simmetrisize :: WeightedGraph a -> WeightedGraph a
simmetrisize g = g ++ [((y,x),c) | ((x,y),c) <- g]

-- todo: add previous map (to reconstruct path back)
dijkstra :: Ord a => WeightedGraph a -> a -> [(a,Cost)]
dijkstra g s = step [(s,0)] M.empty
  where step [] v = M.toAscList v
        step (x@(s,d):xs) v 
          | s `M.member` v = step xs v
          | otherwise      = let qs = sortBy (comparing snd) $ xs ++ expand x
                             in  step qs (insert x v)
        expand (s,d)   = [(t,d+c) | (t,c) <- succ g s]
        insert (s,d) v = M.alter f s v
          where f Nothing   = Just d
                f (Just d') = Just $ min d d'

ucSearch :: (Ord a) => WeightedGraph a -> a -> (a -> Bool) -> [(a,Cost)]
ucSearch g s0 goal = step [(s0,0,[(s0,0)])] S.empty
  where step [] v = []
        step (x@(s,d,ss):xs) v 
          | goal s       = reverse ss
          | S.member s v = step xs v
          | otherwise    = let qs = sortBy (comparing cost) $ xs ++ expand x
                           in  step qs (S.insert s v)
        expand (s,d,ss) = [(t,d+c,(t,c):ss) | (t,c) <- succ g s]
        cost (_,g,_) = g

succ :: Eq a => WeightedGraph a -> a -> [(a,Cost)]
succ g s1 = [ (s2,c) | ((s1',s2),c) <- g, s1' == s1 ] 

g = [
  (('a','b'),1), (('b','a'),1),(('b','c'),3), (('a','c'),10), 
  (('a','d'),15), (('c','d'),5), (('e','a'),1)]

---
-- TODO: generic graph API!!!!
-- insertEdge...
--
-------

type DRelWeighting = DPatternLabel -> LP -> LP -> Double

weightedDerivGraph :: DRelWeighting -> [LP] -> WeightedGraph LP
weightedDerivGraph w ls = [((l1,l2),w d l1 l2) | ((l1,l2),d) <- derivGraph ls]

derivLists :: DRelWeighting -> [LP] -> [(LP,[(LP,Double)])]
derivLists w ls@(_:_:_)  = [ (l1,xs) | l1 <- ls,
    let xs = map (\(l2,c) -> (l2,1/c)) . 
             sortBy (comparing snd) . filter ((>0) . snd) $ dijkstra dg l1,
    not $ null xs ]
  where dg = simmetrisize $ weightedDerivGraph w ls
derivLists _ _ = []

constWeighting :: DRelWeighting
constWeighting _ _ _ = 1

confWeighting :: [(DPatternLabel,Int)] -> DRelWeighting
confWeighting xs = (\d _ _ -> case lookup d xs of
  Nothing -> 0
  Just c  -> realToFrac $ 4 - c)

------------------------------------------------------------------------------
------------------------------------------------------------------------------

type DerivPairs = M.Map LP [(LP,Double)]  
-- TODO: replace with graph! this is a weighted (labeled) graph!

readDerivPairs :: FilePath -> IO DerivPairs
readDerivPairs f = 
  (M.fromList . map (parse . words) . lines) `liftM` readFile f
  where parse (l:xs) = let ls = [(readLP l,read w) | (l,w) <- pairUp xs]
                       in (readLP . init $ l,ls)
 
pairUp :: [a] -> [(a, a)]       
pairUp (x:y:xs) = (x,y):pairUp xs
pairUp _ = []

-- closure
derivPairs :: FilePath -> IO (LP -> [(LP,Double)])
derivPairs f = do
  dl <- readDerivPairs f
  return (\l -> case M.lookup l dl of
    Nothing -> []
    Just ls -> ls)

derivations :: DerivPairs -> LP -> [(LP,Double)]
derivations dp l = M.findWithDefault [] l dp

derivConf :: DerivPairs -> LP -> LP -> Double
derivConf dp l1 l2 = fromMaybe 0 $ lookup l2 (derivations dp l1)


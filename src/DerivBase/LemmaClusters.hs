{-------------------------------------------------------------------------------

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

-- todo: parametriziraj s obzirom na pos
-- todo: promijeni naziv modula, npr. LemmaContainers

module DerivBase.LemmaClusters where

import Data.List
import Data.Ord
import qualified Data.ByteString.UTF8 as B
import Control.Monad
import Utils.Misc
import qualified Data.Map as M
import Data.Maybe

type Lemma = B.ByteString
type LP    = (Lemma,POS)
type LPC   = (Lemma,POS,Int)
data POS   = Nm | Nf | Nn | N | A | V
  deriving (Eq,Ord,Show,Read)

nounPos = [Nm,Nf,Nn] -- <-- izbaci ovo, nakon što parametriziraš s obzirom na POS

readClusters :: FilePath -> IO LemmaClusters
readClusters f = 
  (mkLemmaClusters . map parseCluster . lines) `liftM` readFile f
  where parseLP s   = let (l,_:p) = break (=='_') s in 
                          (B.fromString l,read p)
        parseCluster = map parseLP . words

showClusters :: LemmaClusters -> String
showClusters = unlines . map (unwords . map showLP) . clusters

showLP :: LP -> String
showLP (l,p) = B.toString l ++ "_" ++ show p

showClusters' :: [[LP]] -> String
showClusters' = unlines . map (unwords . map showLP)

clusterSizeDistribution :: LemmaClusters -> [(Int,Int)]
clusterSizeDistribution lpss =  
  [ (x,length xs) | xs@(x:_) <- group . sort . map length $ clusters lpss ]

readLemmaCounts :: FilePath -> IO LemmaCounts
readLemmaCounts f = (mkLemmaCounts . map parse . lines) `liftM` readFile f
  where parse s = case words s of
          [l,p,n] -> (B.fromString l,read p,read n)
          _       -> error "parse error"

data LemmaCounts = LF {
  lfMap :: M.Map LP Int }
  deriving (Eq,Show,Read,Ord)

mkLemmaCounts :: [LPC] -> LemmaCounts
mkLemmaCounts ls =
  LF { lfMap = M.fromList [((l,p),c) | (l,p,c) <- ls] }

lemmaCount :: LemmaCounts -> LP -> Int
lemmaCount lf l = M.findWithDefault 0 l (lfMap lf)

sampleLemmas :: Int -> LemmaCounts -> IO LemmaCounts
sampleLemmas n ls = 
  mkLemmaCounts `liftM` (chooseNDistinctFromList n $ counts ls)

sampleLemmasSuchThat :: 
  (LP -> Bool) -> Int -> [LP] -> IO [LP]
sampleLemmasSuchThat p n ls = 
  chooseNFromListSuchThat (\l ls -> p l && l `notElem` ls) n ls

{-
sampleLemmasWithFreq :: 
  (Int,Int) -> Int -> LemmaCounts -> IO [LP]
sampleLemmasWithFreq (f1,f2) n ls = 
  sampleLemmasSuchThat (\(_,_,c) -> f1 <= c && c <= f2) n (counts ls)
-}

counts :: LemmaCounts -> [LPC]
counts = map (\((l,p),c) -> (l,p,c)) . M.assocs . lfMap

instance LemmaContainer LemmaCounts where
  lemmas = M.keys . lfMap

class LemmaContainer a where
  lemmas :: a -> [LP]

sortClusters :: ([LP] -> [LP] -> Ordering) -> LemmaClusters -> LemmaClusters
sortClusters p = mkLemmaClusters . sortBy p . clusters

sampleClusters :: Int -> LemmaClusters -> IO LemmaClusters
sampleClusters n cs = 
  mkLemmaClusters `liftM` chooseNDistinctFromList n (clusters cs)

sampleClustersSuchThat :: 
  ([LP] -> Bool) -> Int -> LemmaClusters -> IO LemmaClusters
sampleClustersSuchThat p n cs = 
  mkLemmaClusters `liftM` chooseNDistinctFromList n (filter p $ clusters cs)

filterClusters :: ([LP] -> Bool) -> LemmaClusters -> LemmaClusters
filterClusters p = mkLemmaClusters . filter p . clusters

filterLemmaCounts :: (LPC -> Bool) -> LemmaCounts -> LemmaCounts
filterLemmaCounts p = mkLemmaCounts . filter p . counts

readLPs :: FilePath -> IO [LP]
readLPs f = (map readLP . lines) `liftM` readFile f

readLP :: String -> LP
readLP s = case break (=='_') s of
             (l,_:p) -> (B.fromString l,read p)
             _       -> error "parse error"

{-
readLPs' :: FilePath -> IO [(B.ByteString,POS)]
readLPs' f = (map parse . words) `liftM` readFile f
  where parse s = case break (==/'_') s of
          (l,_:p) -> (B.fromString l,read p)
          _       -> error "parse error"
-}

mkLemmaClusters :: [[LP]] -> LemmaClusters
mkLemmaClusters lss = LC { icMap = m1, liMap = m2 }
  where m1 = M.fromList $ zip [1..] lss
        m2 = M.fromList [(l,ix) | (ix,ls) <- zip [1..] lss, l <- ls]

data LemmaClusters = LC {
  icMap :: M.Map Int [LP],
  liMap :: M.Map LP Int }
  deriving (Eq,Show,Read,Ord)

clusters :: LemmaClusters -> [[LP]]
clusters = M.elems . icMap

instance LemmaContainer LemmaClusters where
  lemmas = M.keys . liMap

lemmaCluster :: LemmaClusters -> LP -> [LP]
lemmaCluster cs l = case M.lookup l (liMap cs) of
  Nothing -> []
  Just ix -> case M.lookup ix (icMap cs) of
               Nothing -> []
               Just ls -> ls

lemmaClusterIndex :: LemmaClusters -> LP -> Maybe Int
lemmaClusterIndex cs l = M.lookup l (liMap cs) 

sameCluster :: LemmaClusters -> LP -> LP -> Bool
sameCluster cs l1 l2 = isJust i1 && i1 == i2
  where i1 = lemmaClusterIndex cs l1
        i2 = lemmaClusterIndex cs l2


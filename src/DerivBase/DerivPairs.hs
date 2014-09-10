{-------------------------------------------------------------------------------

 DerivBase.DerivPairs
 (c) 2014 Jan Snajder <jan.snajder@fer.hr>
 
 Assumption: Edges are symmetric.

-------------------------------------------------------------------------------}

module DerivBase.DerivPairs
  ( DerivPairs
  , fromList
  , toList
  , fromFile
  , toFile
  , derivNeighbors
  , derivSet
  , sameDerivFamily ) where

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import DerivBase.Lemma

type DerivPairs l = Map l [(l,Double)]  

fromList :: (Ord l, Lemma l) => [(l, l, Double)] -> DerivPairs l
fromList = ins M.empty . nub . sym
  where parse (l1:l2:w:_) = ((l1, l2), read w)
        parse _           = error "no parse"
        ins = foldl' (\m (l1, l2, w) -> 
                M.insertWith (++) l1 [(l2, w)] m)
        sym = concatMap (\x@(w1, w2, w) -> [x, (w2, w1, w)])

toList :: DerivPairs l -> [(l, l, Double)]
toList dp = [ (l1, l2, w) | (l1,xs) <- M.toAscList dp, (l2,w) <- xs ]

derivNeighbors :: (Ord l, Lemma l) => DerivPairs l -> l -> [(l, Double)]
derivNeighbors dp l = sort . fromMaybe [] $ M.lookup l dp

derivSet :: (Ord l, Lemma l) => DerivPairs l -> l -> [l]
derivSet dp l = sort . fromMaybe [] $ ((l:) . map fst) <$> M.lookup l dp

sameDerivFamily :: (Ord l, Lemma l) => DerivPairs l -> l -> l -> Bool
sameDerivFamily dp l1 l2 = not (null ds1) && ds1 == ds2
  where ds1 = derivSet dp l1
        ds2 = derivSet dp l2 

fromFile :: (Ord l, Lemma l) => FilePath -> IO (DerivPairs l)
fromFile f = fromList . map (parse . words) . lines <$> readFile f 
  where parse (l1:l2:w:_) = (readLemma l1, readLemma l2, read w)
        parse _           = error "no parse"

toFile :: (Lemma l) =>  FilePath -> DerivPairs l -> IO ()
toFile f = writeFile f . unlines . map showPair . toList
  where showPair (l1, l2, w) = unwords [showLemma l1, showLemma l2, show w]


{-------------------------------------------------------------------------------

 DerivBase.DerivPairs
 (c) 2014 Jan Snajder <jan.snajder@fer.hr>
 
-------------------------------------------------------------------------------}

module DerivBase.DerivPairs
  ( DerivPairs
  , fromFile
  , toFile
  , derivLinks
  , derivSet
  , sameDerivFamily ) where

import Control.Applicative
import Data.Function (on)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
import DerivBase.Lemma

type Weight = Double
type DerivPairs l = Map l [(l, Weight)]

fromFile :: (Ord l, Lemma l) => FilePath -> IO (DerivPairs l)
fromFile f = 
  M.fromList . map (parse . words) . lines <$> readFile f
  where parse (l:xs) = let ls = [(readLemma l,read w) | (l,w) <- pairUp xs]
                       in (readLemma l,ls)

pairUp :: [a] -> [(a, a)]       
pairUp (x:y:xs) = (x,y):pairUp xs
pairUp _ = []

derivLinks :: (Ord l, Lemma l) => DerivPairs l -> l -> [(l, Weight)]
derivLinks dp l = sort . fromMaybe [] $ M.lookup l dp

derivSet :: (Ord l, Lemma l) => DerivPairs l -> l -> [l]
derivSet dp l = sort . fromMaybe [] $ ((l:) . map fst) <$> M.lookup l dp

sameDerivFamily :: (Ord l, Lemma l) => DerivPairs l -> l -> l -> Bool
sameDerivFamily dp l1 l2 = not (null ds1) && ds1 == ds2
  where ds1 = derivSet dp l1
        ds2 = derivSet dp l2 

{-
fromFile :: (Ord l, Lemma l) => FilePath -> IO (DerivPairs l)
fromFile f = fromList . map (parse . words) . lines <$> readFile f 
  where parse (l1:l2:w:_) = (readLemma l1, readLemma l2, read w)
        parse _           = error "no parse"
-}

toFile :: (Lemma l) => FilePath -> DerivPairs l -> IO ()
toFile f = writeFile f . unlines . map showLine . M.toList
  where showLine (l1,ls) = unwords $ 
          showLemma l1 : concatMap (\(l2,w) -> [showLemma l2, show w]) ls


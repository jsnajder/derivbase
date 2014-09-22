{-------------------------------------------------------------------------------

 DerivBase.DerivPairs
 (c) 2014 Jan Snajder <jan.snajder@fer.hr>
 
-------------------------------------------------------------------------------}

module DerivBase.DerivPairs
  ( DerivPairs
  , fromFile
  , toFile
  , toList
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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import DerivBase.LemmaItem

type Weight = Double
type DerivPairs l = Map l [(l, Weight)]

fromFile :: (Ord l, LemmaItem l) => FilePath -> IO (DerivPairs l)
fromFile f = 
  M.fromList . map (parse . T.words) . T.lines <$> T.readFile f
  where 
    parse (l:xs) = let ls = [(readLemmaItem l2, read' w) | 
                              (l2,w) <- sort $ pairUp xs]
                   in (readLemmaItem l,ls)
    read' s = case T.rational s of
                Right (n,_) -> n
                Left e      -> error e

pairUp :: [a] -> [(a, a)]       
pairUp (x:y:xs) = (x,y):pairUp xs
pairUp _ = []

derivLinks :: (Ord l, LemmaItem l) => DerivPairs l -> l -> [(l, Weight)]
derivLinks dp l = fromMaybe [] $ M.lookup l dp

derivSet :: (Ord l, LemmaItem l) => DerivPairs l -> l -> [l]
derivSet dp l = fromMaybe [] $ ((l:) . map fst) <$> M.lookup l dp

sameDerivFamily :: (Ord l, LemmaItem l) => DerivPairs l -> l -> l -> Bool
sameDerivFamily dp l1 l2 = not (null ds1) && ds1 == ds2
  where 
    ds1 = derivSet dp l1
    ds2 = derivSet dp l2 

toFile :: (LemmaItem l) => FilePath -> DerivPairs l -> IO ()
toFile f = T.writeFile f . T.unlines . map showLine . M.toList
  where 
    showLine (l1,ls) = T.unwords $ showLemmaItem l1 : 
      concatMap (\(l2,w) -> [showLemmaItem l2, T.pack $ show w]) ls

toList :: (LemmaItem l) => DerivPairs l -> [(l, [(l, Weight)])]
toList = M.toList


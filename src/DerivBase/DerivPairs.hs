{-------------------------------------------------------------------------------

 DerivBase.DerivPairs
 (c) 2014 Jan Snajder <jan.snajder@fer.hr>
 
 Assumption: Edge are symmetric.

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
import DerivBase.LemmaPos

type DerivPairs lp = Map lp [(lp,Double)]  

fromList :: (Ord lp, LemmaPos lp) => [(lp, lp, Double)] -> DerivPairs lp
fromList = ins M.empty . nub . sym
  where parse (l1:l2:w:_) = ((l1, l2), read w)
        parse _           = error "no parse"
        ins = foldl' (\m (l1, l2, w) -> 
                M.insertWith (++) l1 [(l2, w)] m)
        sym = concatMap (\x@(w1, w2, w) -> [x, (w2, w1, w)])

toList :: DerivPairs lp -> [(lp, lp, Double)]
toList dp = [ (l1, l2, w) | (l1,xs) <- M.toAscList dp, (l2,w) <- xs ]

derivNeighbors :: (Ord lp, LemmaPos lp) => DerivPairs lp -> lp -> [(lp, Double)]
derivNeighbors dp l = sort . fromMaybe [] $ M.lookup l dp

derivSet :: (Ord lp, LemmaPos lp) => DerivPairs lp -> lp -> [lp]
derivSet dp l = sort . fromMaybe [] $ ((l:) . map fst) <$> M.lookup l dp

sameDerivFamily :: (Ord lp, LemmaPos lp) => 
  DerivPairs lp -> lp -> lp -> Bool
sameDerivFamily dp l1 l2 = not (null ds1) && ds1 == ds2
  where ds1 = derivSet dp l1
        ds2 = derivSet dp l2 

fromFile :: (Ord lp, LemmaPos lp) => FilePath -> IO (DerivPairs lp)
fromFile f = fromList . map (parse . words) . lines <$> readFile f 
  where parse (l1:l2:w:_) = (lpRead l1, lpRead l2, read w)
        parse _           = error "no parse"

toFile :: (LemmaPos lp) =>  FilePath -> DerivPairs lp -> IO ()
toFile f = writeFile f . unlines . map showPair . toList
  where showPair (l1, l2, w) = unwords [lpShow l1, lpShow l2, show w]


{-------------------------------------------------------------------------------

 DerivBase.DerivSets
 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module DerivBase.DerivSets
  ( DerivSets
  , fromList
  , toList
  , fromFile
  , toFile
  , derivSet
  , derivFamilyId
  , sameDerivFamily ) where

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import DerivBase.LemmaItem

data DerivSets l = DerivSets
  { isMap :: Map Int [l]
  , liMap :: Map l Int 
  } deriving (Eq, Show, Read, Ord)

fromList :: (Ord l, LemmaItem l) => [[l]] -> DerivSets l
fromList lss = DerivSets { isMap = m1, liMap = m2 }
  where m1 = M.fromList . zip [1..] $ map sort lss
        m2 = M.fromList [(l,ix) | (ix,ls) <- zip [1..] lss, l <- ls]

toList :: DerivSets l -> [[l]]
toList = M.elems . isMap

derivSet :: (Ord l, LemmaItem l) => DerivSets l -> l -> [l]
derivSet ds l = 
  fromMaybe [] $ M.lookup l (liMap ds) >>= \ix -> M.lookup ix (isMap ds)

derivFamilyId :: (Ord l, LemmaItem l) => DerivSets l -> l -> Maybe Int
derivFamilyId ds l = M.lookup l (liMap ds)

sameDerivFamily :: (Ord l, LemmaItem l) => DerivSets l -> l -> l -> Bool
sameDerivFamily ds l1 l2 = isJust i1 && i1 == i2
  where i1 = derivFamilyId ds l1
        i2 = derivFamilyId ds l2

fromFile :: (Ord l, LemmaItem l) => FilePath -> IO (DerivSets l)
fromFile f = 
  fromList . map parseFamily . T.lines <$> T.readFile f
  where parseFamily = map readLemmaItem . T.words

toFile :: (LemmaItem l) =>  FilePath -> DerivSets l -> IO ()
toFile f = 
  T.writeFile f . T.unlines . map (T.unwords . map showLemmaItem) . toList


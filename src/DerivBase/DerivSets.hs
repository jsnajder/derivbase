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
  , derivFamily
  , derivFamilyId
  , sameDerivFamily ) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import DerivBase.LemmaPos

data DerivSets lp = DerivSets
  { isMap :: Map Int [lp]
  , liMap :: Map lp Int 
  } deriving (Eq, Show, Read, Ord)

fromList :: (Ord lp, LemmaPos lp) => [[lp]] -> DerivSets lp
fromList lss = DerivSets { isMap = m1, liMap = m2 }
  where m1 = M.fromList $ zip [1..] lss
        m2 = M.fromList [(l,ix) | (ix,ls) <- zip [1..] lss, l <- ls]

toList :: DerivSets lp -> [[lp]]
toList = M.elems . isMap

derivFamily :: (Ord lp, LemmaPos lp) => DerivSets lp -> lp -> [lp]
derivFamily ds l = 
  fromMaybe [] $ M.lookup l (liMap ds) >>= \ix -> M.lookup ix (isMap ds)

derivFamilyId :: (Ord lp, LemmaPos lp) => DerivSets lp -> lp -> Maybe Int
derivFamilyId ds l = M.lookup l (liMap ds)

sameDerivFamily :: (Ord lp, LemmaPos lp) => 
  DerivSets lp -> lp -> lp -> Bool
sameDerivFamily ds l1 l2 = isJust i1 && i1 == i2
  where i1 = derivFamilyId ds l1
        i2 = derivFamilyId ds l2

fromFile :: (Ord lp, LemmaPos lp) => FilePath -> IO (DerivSets lp)
fromFile f = 
  fromList . map parseFamily . lines <$> readFile f
  where parseFamily = map lpRead . words

toFile :: (LemmaPos lp) =>  FilePath -> DerivSets lp -> IO ()
toFile f = writeFile f . unlines . map (unwords . map lpShow) . toList


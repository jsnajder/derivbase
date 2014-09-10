{-------------------------------------------------------------------------------

 DerivBase
 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module DerivBase 
  ( module DerivBase.DerivPairs
  , module DerivBase.DerivSets
  , module DerivBase.LemmaPos ) where

--import DerivBase.DerivGraph
import qualified DerivBase.DerivPairs
import qualified DerivBase.DerivSets
import DerivBase.LemmaPos

------------------------------------------------------------------------------
-- tmp:

data POS   = Nm | Nf | Nn | N | A | V deriving (Show, Read, Eq, Ord, Enum)
instance Pos POS where
  posShow   = show
  posRead   = read
  posValues = [toEnum 0..]

--load = DS.fromFile "/home/jan/dismods/derivbase/deriv-families/data/DErivBase-v1.4.1-families.txt" :: IO (DS.DerivSets (LP POS))

-- derivFamily df (LP "Wasser" Nn)


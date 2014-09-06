{-------------------------------------------------------------------------------

 DerivBase.LemmaPos
 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses #-}

module DerivBase.LemmaPos where

class Pos p where
  posShow   :: p -> String
  posRead   :: String -> p
  posValues :: [p]

class LemmaPos lp where
  lp     :: String -> String -> lp
  lemma  :: lp -> String
  pos    :: lp -> String
  lpShow :: lp -> String
  lpRead :: String -> lp

  lpShow lp = lemma lp ++ "_" ++ pos lp

  -- todo: make parsing more robust
  lpRead s = let (l,_:p) = break (=='_') s in lp l p

data LP p = LP String p deriving (Show, Ord, Eq)

instance Pos p => LemmaPos (LP p) where
  lp l p         = LP l (posRead p)
  lemma (LP l _) = l
  pos   (LP _ p) = posShow p


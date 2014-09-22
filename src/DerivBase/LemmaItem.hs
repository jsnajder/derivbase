{-------------------------------------------------------------------------------

 DerivBase.LemmaItem
 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, OverloadedStrings #-}

module DerivBase.LemmaItem
  ( LemmaItem (..)
  , Lemma
  , LemmaPos (LP)
  , LemmaPosOpt (LPO)
  , removePos
  , setPos ) where

import Data.Text (Text)
import qualified Data.Text as T

class LemmaItem l where
  readLemmaItem :: Text -> l
  showLemmaItem :: l -> Text
  lemma         :: l -> Text

type Lemma = Text

instance LemmaItem Lemma where
  readLemmaItem = id
  showLemmaItem = id
  lemma         = id

------------------------------------------------------------------------------

-- Lemma-POS
data LemmaPos p = LP Lemma p deriving (Show, Ord, Eq)

instance (Read p, Show p) => LemmaItem (LemmaPos p) where

  readLemmaItem s = case T.splitOn "_" s of
                      [l,p] -> LP l (read $ T.unpack p)
                      _     -> error "no parse"

  showLemmaItem (LP l p) = T.concat [l, "_", T.pack $ show p]

  lemma (LP l p) = l

-- Lemma-POS with an optional POS tag
data LemmaPosOpt p = LPO Lemma (Maybe p) deriving (Show, Ord, Eq)

instance (Read p, Show p) => LemmaItem (LemmaPosOpt p) where
  
  readLemmaItem s = case T.splitOn "_" s of
                      [l,p] -> LPO l . Just . read $ T.unpack p
                      [l]   -> LPO l Nothing
                      _     -> error "no parse"

  showLemmaItem (LPO l Nothing)  = l
  showLemmaItem (LPO l (Just p)) = T.concat [l, "_", T.pack $ show p]

  lemma (LPO l _ ) = l


removePos :: LemmaPosOpt p -> LemmaPosOpt p
removePos (LPO l _) = LPO l Nothing

setPos :: p -> LemmaPosOpt p -> LemmaPosOpt p
setPos p (LPO l _) = LPO l (Just p)


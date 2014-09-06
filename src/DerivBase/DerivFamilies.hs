{-------------------------------------------------------------------------------

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

-- TODO: kaotično! srediti!

-- u vezi N vs Nm/f/n konflacije, napravi ovako: funkcija koja ekspandira i
-- zatim sažima nakon derivacije. To neka bude closure funkcija koja u sebi
-- ima skup lemma s N oznakom. Ta funkcija ne ulazi u ovaj modul, nego se
-- eksterno pripremi!

-- TODO: Razdvoji: DerivFamilies, DerivGraph, DerivLists


module DerivBase.DerivFamilies (
  derivFamilies,
  derivChain,
  patternPairs,
  WeightedGraph,
  Cost,
  DPatternLabel,
  derivGraph) where

import Prelude hiding (succ)
import MorphGrammar.Hofm.Language.German14 -- TODO: apstrahiraj!
import Clustering.Partition (eqClassesGenOut)
import Data.List
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Maybe
import Data.Char
import qualified Data.ByteString.UTF8 as B
import Control.Monad
import System.Environment
import DerivBase.LemmaClusters
import qualified Data.Map as M

derivations :: (String,IPatternDefault) -> [(String,IPatternDefault)]
derivations (l1,ip1) = 
  [(l2,ip2) | d <- dPatterns::[DPatternDefault], 
              (l2,ips) <- lDerive d (l1,ip1), ip2 <- ips]

getPatterns :: POS -> [IPatternDefault]
getPatterns N  = nouns
getPatterns Nm = [mNoun]
getPatterns Nf = [fNoun]
getPatterns Nn = [nNoun]
getPatterns A  = [adjective]
getPatterns V  = verbs 

-- s is a set of lemmas with underspecified POS
-- TODO: not only nouns (in principle)
derivations' :: S.Set Lemma -> LP -> [LP]
derivations' s (l1,p1) = [ (l2',p2) |
    ip1 <- getPatterns p1,
    (l2,ip2) <- derivations (lowerIt $ B.toString l1,ip1),
    let l2' = B.fromString $ (if ip2 `elem` nouns then upperIt else id) l2, 
    let p2 = downcastPOS l2' . read $ label ip2 ] 
  where downcastPOS l p | p `elem` nounPos && l `S.member` s = N
                        | otherwise = p
 
lowerIt :: String -> String 
lowerIt (c:cs) = toLower c : cs

upperIt :: String -> String 
upperIt (c:cs) = toUpper c : cs
                                             
-- s is a set of lemmas with underspecified POS
derivFamilies :: S.Set Lemma -> [LP] -> LemmaClusters
derivFamilies s = mkLemmaClusters . eqClassesGenOut (derivations' s)

-- derivation within a deriv. family
deriveWithin :: [LP] -> LP -> [(DPatternLabel,LP)]
deriveWithin ls (l1,p1) = nub [ (label d,(l2,p2)) | 
  ip1 <- getPatterns p1,
  d <- dPatterns,
  (l2',ip2) <- lDerive' d (lowerIt $ B.toString l1,ip1),
  let l2 = B.fromString $ (if ip2 `elem` nouns then upperIt else id) l2',
  let p2 = downcastPOS (l2,read $ label ip2),
  (l2,p2) `elem` ls ]
  where downcastPOS (l,p) | p `elem` nounPos && (l,N) `elem` ls = N
                          | otherwise = p

type DPatternLabel = String

patternPairs :: [LP] -> [(DPatternLabel,LP,LP)]
patternPairs lps = sortBy (comparing scnd) . nub $ 
  [(d,lp1,lp2) | 
   lp1 <- lps, (d,(l2,p2)) <- deriveWithin lps lp1, 
   let lp2 = (l2,downcastPOS l2 p2),
   lp2 `S.member` lps']
  where lps' = S.fromList lps
        downcastPOS l p | p `elem` [Nm,Nf,Nn] && (l,N) `S.member` lps' = N
                        | otherwise = p

frst (x,_,_) = x
scnd (_,x,_) = x

lDerive' :: DPatternDefault -> 
  (String,IPatternDefault) -> [(String,IPatternDefault)]
lDerive' d (l1,ip1) = [(l2,ip2) | (l2,ips) <- lDerive d (l1,ip1), ip2 <- ips]

derivedBy :: [LP] -> LP -> LP -> [DPatternLabel]
derivedBy ls l1 l2 = [ d | (d,l2') <- deriveWithin ls l1, l2'==l2]

derivedBy' :: [LP] -> LP -> LP -> [DPatternLabel]
derivedBy' ls lp1 lp2 = derivedBy ls lp1 lp2 ++ derivedBy ls lp2 lp1

-- TODO: remove from here

type Edge a = (a,a)
type LabeledGraph a b = [(Edge a,b)]
type Cost = Double
type WeightedGraph a = LabeledGraph a Cost

type DerivGraph = LabeledGraph LP DPatternLabel

-- deriv. graph of a deriv. family
derivGraph :: [LP] -> DerivGraph
derivGraph ls = [ ((l1,l2),d) | l1 <- ls, (d,l2) <- deriveWithin ls l1]

derivChains :: [LP] -> LP -> LP -> [[DPatternLabel]]
derivChains ls l1 l2 = 
  filter (not . null ) . sequence . map (uncurry (derivedBy' ls)) . pairsSeq $ 
  bfSearch l1 succ (==l2) 
  where succ l = [l2 | ((l1,l2),d) <- g, l1==l] ++ [l1 | ((l1,l2),d) <- g, l2==l]
        g = derivGraph ls

derivChains2 :: [LP] -> LP -> LP -> [[(DPatternLabel,LP,LP)]]
derivChains2 ls l1 l2 = 
  filter (not . null ) . sequence . 
  map (\(l1,l2) -> [(d,l1,l2) | d <- derivedBy' ls l1 l2]) . pairsSeq $ 
  bfSearch l1 succ (==l2) 
  where succ l = [l2 | ((l1,l2),_) <- g, l1==l] ++ [l1 | ((l1,l2),_) <- g, l2==l]
        g = derivGraph ls

derivChain :: LemmaClusters -> LP -> LP -> [DPatternLabel]
derivChain cs l1 l2 
  | null ls   = []
  | otherwise = case derivChains ls l1 l2 of
                  []  -> []
                  c:_ -> c
  where ls = lemmaCluster cs l1

{-
-- zadnja verzija
derivChain2 :: 
  LemmaClusters -> DRelWeighting -> LP -> LP -> ([DPatternLabel],Cost)
derivChain2 cs w l1 l2 = (ch,c)
  where xs = ucSearch dg l1 (==l2) 
        dg = simmetrisize $ weightedDerivGraph w ls
        ls = lemmaCluster cs l1
        c  = (1/) . realToFrac . sum $ map snd xs
        ch = zipWith (\(l1,_) (l2,_) -> head $ derivedBy' ls l1 l2) xs (tail xs)
-}

pairsSeq :: [a] -> [(a,a)]
pairsSeq xs = zipWith (,) xs (tail xs)

bfSearch :: (Ord a) => a -> (a -> [a]) -> (a -> Bool) -> [a]
bfSearch s0 succ goal = step [(s0,[])] S.empty
  where step [] v = []
        step (x@(s,ss):xs) v 
          | goal s       = reverse $ s:ss
          | S.member s v = step xs v
          | otherwise    = step (xs ++ expand x) (S.insert s v)
        expand (s,ss) = [(t,s:ss) | t <- succ s]


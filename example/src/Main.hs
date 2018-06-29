
{-# LANGUAGE FlexibleContexts #-}

module Main (main)
where

import            Control.Monad.Markov
import            Control.Monad
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Data.Map.Strict (Map)
import            Data.Ratio ( (%), Ratio(..) )


mkBigramMap :: (Num a, Ord k) => [k] -> Map k (Map k a)
mkBigramMap xs =
  let rawBigrams = zip xs (tail xs)
  in L.foldl' f M.empty rawBigrams
  where
    f :: (Num a, Ord k) => Map k (Map k a) -> (k, k) -> Map k (Map k a)
    f acc (c1, c2) = M.insertWith (M.unionWith (+)) c1 (M.singleton c2 1) acc 

nextWord :: MonadMarkov Char m => m String
nextWord =
  reverse <$> loop "" 
  where
    loop acc = do ch <- nextState
                  if ch == '\n'
                    then return acc
                    else loop (ch : acc)


randWords :: Map Char (Map Char Rational) -> Int -> [String]
randWords bigramMap 0 = []
randWords bigramMap n = tail $ 
    evalMarkovStd (replicateM n nextWord) seed '\n' transition  
  where
    seed = 0

    transition :: Char -> [(Char, Rational)]
    transition ch = M.toList $ bigramMap M.! ch

main :: IO ()
main = do
  bigrams <- mkBigramMap <$> readFile "/usr/share/dict/words"
  putStrLn "enter number of words to generate:"
  n <- read <$> getLine
  putStrLn "generated words:\n"
  mapM_ putStrLn (randWords bigrams n)



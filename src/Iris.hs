{-# LANGUAGE OverloadedStrings #-}
module Iris where

import Data.Csv
import Control.Monad

import RDA hiding (transpose) -- (BitVec(..), (:->)(..))
import qualified RDA as RDA
import RDA.Compositional.Structure

import Data.Bits (bit, zeroBits)

import Data.Function (on)
import Data.Foldable (toList)
import Data.Vector (Vector(..))
import Data.List (sortBy, groupBy, transpose, maximumBy)

import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as BL

import Text.Printf
import Utils

data IrisClass = Setosa | Versicolor | Virginica
  deriving(Eq, Ord, Read, Show, Enum)

instance FromField IrisClass where
  parseField s
    | s == "Iris-setosa" = pure Setosa
    | s == "Iris-versicolor" = pure Versicolor
    | s == "Iris-virginica" = pure Virginica
    | otherwise = mzero

type IrisData = (Double, Double, Double, Double, IrisClass)

loadData :: FilePath -> IO (Either String (Vector IrisData))
loadData file = decode NoHeader <$> BL.readFile file

-------------------------------
-- Preprocessing

normalize :: [Double] -> [Double]
normalize [] = error "normalize of empty list"
normalize xs = fmap (\x -> (x - minx) / (maxx - minx)) xs
  where minx = minimum xs
        maxx = maximum xs

binarizeFeatures :: [[Double]] -> [BitVec Integer 4]
binarizeFeatures xs =
  let normed = transpose . fmap normalize . transpose $ xs
  in  fmap (RDA.concatBits @4 @1 . fmap (RDA.cmpz . (>0.5))) normed

oneHot :: forall b a . (KnownNat b, Enum a) => a -> RDA.BitVec Integer b
oneHot c = bit (fromEnum c)

-- note: this is a little unsafe: you should make sure that you have enough bits
-- @b@ to store every integer value of @a@ you give to this function!
binaryLabel :: forall b a . (KnownNat b, Enum a) => a -> RDA.BitVec Integer b
binaryLabel = RDA.bitVec . fromIntegral . fromEnum

-- | Binarize the iris data with a choice of labeling function (e.g., one-hot)
binarizeWith :: KnownNat b
  => (IrisClass -> BitVec Integer b)
  -> [IrisData]
  -> [(BitVec Integer 4, BitVec Integer b)]
binarizeWith labeller dataset =
  zip (binarizeFeatures . fmap removeLabel $ dataset) (toLabel <$> dataset)
  where
    removeLabel (a,b,c,d,_) = [a,b,c,d]
    toLabel (_,_,_,_,y)     = labeller y

-- Split a dataset into train and test sets, using a function to get the label
-- @c@ from an example @a@.
splitTrainTest :: (Eq c, Ord c) => Int -> (a -> c) -> [a] -> ([a], [a])
splitTrainTest n label =
  foldl1 g . fmap (splitAt n) . groupBy ((==) `on` label) . sortBy (compare `on` label)
  where
    f h = h `on` label
    g (train,test) (train', test') = (train++train', test++test')

-- Run an iris classification experiment, and report its accuracy
runExperiment :: forall b . KnownNat b
  => [IrisData] -- ^ dataset
  -> (IrisClass -> BitVec Integer b) -- ^ labeller
  -> Double
runExperiment dataset labeller =
  let binaryData    = binarizeWith labeller dataset
      -- we use a 50% test, 50% train split (each class has exactly 50 examples)
      (train, test)     = splitTrainTest 25 snd binaryData
      ps = RDA.rda model zeroBits train
  in  accuracy (f $ last ps) test
  where
    model@(fwd :-> rev) = evalModel @4 @b
    -- note: our trained model is "fwd (append p x)" because we need to fix the
    -- learned parameters. In the paper we write this @f(θ, -)@.
    f p x = fwd (append p x)

main = do
  let path = "./data/iris.data"
  dataset <- either quit toList <$> loadData path
  let twoClass = filter (\(_,_,_,_,y) -> y /= Virginica) dataset

  printf "      Empirical Results (Iris)\n"
  printf "-------------------------------------\n"
  printf "Iris (two-class)    binary     %.1f%%\n" (100 * runExperiment @1 twoClass binaryLabel)
  printf "Iris (two-class)    one-hot    %.1f%%\n" (100 * runExperiment @2 twoClass oneHot)
  printf "-------------------------------------\n"
  printf "Iris                binary     %.1f%%\n" (100 * runExperiment @2 dataset  binaryLabel)
  printf "Iris                one-hot    %.1f%%\n" (100 * runExperiment @3 dataset  oneHot)

  return ()
  where quit err = error $ "failed to load data: " ++ err

-- | We implement `eval` slightly differently to the RDA library, so parameters
-- learned from the following model will be permuted with respect to @RDA.eval@.
--
-- >  θ → ----------------------\
-- >                           mul --- repeated @b add --- → y
-- >  x → --- basis -- copyN ---/
--
evalModel :: forall a b . (RDA.KnownNat a, RDA.KnownNat b) => (b*2^a + a) :-> b
evalModel = identity
  `chain` (identity @(2^a * b) `tensor` basis @a)
  `chain` (identity @(2^a * b) `tensor` copyN @(2^a) @b)
  `chain` multiply @(2^a * b)
  `chain` repeated @b (addN @1 @(2^a))

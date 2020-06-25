module MNIST where

import Text.Printf
import GHC.Word
import qualified Data.ByteString as BS
import Data.Bits
import Data.Function (on)
import Data.List (maximumBy)

import Data.MNIST
import RDA
import Utils

-- NOTE: this is the mean pixel value of the MNIST dataset,
-- which we use to binarize the dataset.
meanPixelValue :: Word8
meanPixelValue = 32

-- | Turn an image into a @28*28@ bitvector
binarize :: BS.ByteString -> BitVec Integer 784 -- 784 = 28 * 28
binarize = BS.foldr (\w a -> (a `shiftL` 1) .|. wordToBit w) zeroBits
  where wordToBit = cmpz . (> meanPixelValue)

-- | one-hot-encode a label
oneHot :: forall n . KnownNat n => Word8 -> BitVec Integer n
oneHot = bit . fromIntegral

-- | Load the MNIST dataset and turn it into bitvectors
loadDataset :: FilePath -> FilePath -> IO [(BitVec Integer (28*28), BitVec Integer 1)]
loadDataset imagesPath labelsPath = do
  (images, labels) <- readMnistDataset imagesPath labelsPath
  -- two-class subset of MNIST
  let dataset   = filter (\(x,y) -> y <= 1) $ zip (_imagesPixels images) (_labelsLabels labels)
      binarized = fmap (\(x,y) -> (binarize x, cmpz y)) dataset
  return binarized

main = do
  -- MNIST is pre-split into train/test sets.
  train <- loadDataset "./data/train-images-idx3-ubyte" "./data/train-labels-idx1-ubyte"
  test  <- loadDataset "./data/t10k-images-idx3-ubyte" "./data/t10k-labels-idx1-ubyte"

  -- note on magic numbers below:
  -- @784@    is the input dimensionality: 28 * 28
  -- @11400@  specifies a train/validation split of about 95% / 5%
  -- @4@      is a hyperparameter to pseudoLinear- see implementation.
  let model  = pseudoLinear @784 4
      scored = runExperiment @784 @784 11400 model train

  let bestParams = fst (last scored)
      (fwd :-> _) = model
      f p x = fwd (append p x)

  printf "      Empirical Results (MNIST)\n"
  printf "-------------------------------------\n"
  printf "MNIST               binary     %.1f%%\n" (100 * accuracy (f bestParams) test :: Double)

-------------------------------
-- Experiment / Training helpers

-- Train the model using a validation set to pick the best parameters
runExperiment :: forall a p b . (KnownNat a, KnownNat p, KnownNat b)
    => Int
    -> ((p + a) :-> b) -- ^ model
    -> [(BitVec Integer a, BitVec Integer b)]
    -> [(BitVec Integer p, Int)]
runExperiment holdout model dataset =
  let (train, validate) = splitAt holdout dataset
      scored = take 1000 $ trainValidated @p model zeroBits train validate
  in  scored
  --in  fst $ maximumBy (compare `on` snd) scored

-- We train using RDA, but keep track of the best perfoming set of parameters
-- to use on the test set.
trainValidated :: forall p a b r . (KnownNat p, KnownNat a, KnownNat b, Ord r, Num r)
  => ((p + a) :-> b)
  -> BitVec Integer p
  -> [(BitVec Integer a, BitVec Integer b)]
  -> [(BitVec Integer a, BitVec Integer b)]
  -> [(BitVec Integer p, r)]
trainValidated model params dataset validation = keepMaxBy totalCorrect ps
  where
    ps = rda model params dataset
    f p x = let (fwd :-> _) = model in fwd (append p x)
    totalCorrect p = Prelude.sum [ if f p x == y then 1 else 0 | (x, y) <- validation ]

-- | annotate each item in a list with a score, and keep the maximum score seen
-- so far.
keepMaxBy :: Ord r => (a -> r) -> [a] -> [(a, r)]
keepMaxBy score = scanl1 f . fmap (\x -> (x, score x))
  where f a b = if snd a > snd b then a else b

-------------------------------
-- Model code

-- | The 'pseudoLinear' model from the paper
--
-- NOTE: n is a hyperparameter essentially saying what ratio of output popcount
-- to mask popcount we should look for.
pseudoLinear :: forall a . KnownNat a => Int -> (a + a) :-> 1
pseudoLinear n = fwd :-> rdiffB fwd
  where
    fwd v =
      let (p, x) = split @a @a v
      -- fewer than 25% bits of the mask were matched, so this is probably a 1
      -- (we are trying to learn our mask to be the 'average 0')
      in cmpz $ popCount (p .&. x) <= (popCount p `div` n)

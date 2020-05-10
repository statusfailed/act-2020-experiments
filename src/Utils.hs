module Utils where

-- Compute the accuracy of a trained model on some data.
accuracy :: Eq y => (x -> y) -> [(x,y)] -> Double
accuracy trainedModel examples =
  let dy = fmap (\(x,y) -> y == trainedModel x) examples
  in  (fromIntegral $ length (filter id dy)) / fromIntegral (length dy)


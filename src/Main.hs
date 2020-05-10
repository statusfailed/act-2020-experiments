module Main where

import System.Environment
import qualified Iris
import qualified MNIST

runIris :: IO ()
runIris = Iris.main

runMnist :: IO ()
runMnist = MNIST.main

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["iris"] -> runIris
    ["mnist"] -> runMnist
    _ -> usage

usage :: IO ()
usage = do
  this <- getProgName
  putStrLn $ "usage: " ++ this ++ " [iris|mnist]"

module Main where

import System.Environment
import qualified Iris

runIris :: IO ()
runIris = Iris.main

runMnist :: IO ()
runMnist = putStrLn "TODO: mnist"

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

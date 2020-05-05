module Main where

import System.Environment

runIris :: IO ()
runIris = putStrLn "TODO: iris"

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

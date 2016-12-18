module Reverse where

rvrs :: String -> String
rvrs phrase =
  drop 9 phrase ++
    take 4 (drop 5 phrase) ++
      take 5 phrase

main :: IO ()
main =
  print $ rvrs "Curry is awesome"

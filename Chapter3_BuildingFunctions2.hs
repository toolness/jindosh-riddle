module Boop where

addExc :: String -> String
addExc x = x ++ "!"

get5th :: String -> String
get5th x = [x !! 4]

getLastPart :: String -> String
getLastPart x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs = do
  drop 9 phrase ++
    take 4 (drop 5 phrase) ++
      take 5 phrase
  where phrase = "Curry is awesome"

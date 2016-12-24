module Display
  ( display
  , displayMany
  ) where

import Text.Printf
import Data.Maybe

import Person

-- The following display code is super weird because I don't know what
-- Monads are yet, and things that I thought would work--like
-- issuing a `printf` in a function called by `map`--mysteriously
-- don't. Because side effects are bad, I guess? I have no idea.
display :: [Person] -> IO ()
display people =
  let
    getPropValue prop person =
      let
        value = ((get prop) person)
      in
        if value == Nothing then "??" else show (fromJust value)
    displayProp :: (Show x, Eq x) => Property x -> Person -> IO ()
    displayProp prop person =
      do
        printf "%20s " (getPropValue prop person)
    displayRow prop people =
      if null people then
        printf "\n"
        else
          do
            displayProp prop (head people)
            displayRow prop (tail people)
  in
    do
      displayRow nameProp people
      displayRow heirloomProp people
      displayRow drinkProp people
      displayRow originProp people
      displayRow colorProp people
      displayRow positionProp people

displayMany :: [[Person]] -> IO ()
displayMany candidates =
  if null candidates then return () else
    do
      display (head candidates)
      printf "\n"
      displayMany (tail candidates)

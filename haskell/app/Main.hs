module Main where

import Text.Printf

import Person
import Constraint
import Solver
import Display

constraints :: [Constraint]
constraints = [ simpleConstraint nameProp Contee colorProp Red
              , simpleConstraint positionProp FarLeft nameProp Natsiou
              , simpleConstraint positionProp SecondFromLeft colorProp Green
              , simpleConstraint positionProp Center drinkProp Beer
              , simpleConstraint drinkProp Wine colorProp Purple
              , simpleConstraint originProp Dabokva colorProp White
              , simpleConstraint nameProp Winslow heirloomProp Diamond
              , simpleConstraint originProp Baleton heirloomProp Ring
              , simpleConstraint nameProp Finch drinkProp Absinthe
              , simpleConstraint originProp Dunwall drinkProp Whiskey
              , simpleConstraint nameProp Marcolla originProp Fraeport
              , neighborConstraint heirloomProp Tin originProp Dabokva
              , neighborConstraint heirloomProp Medal originProp Karnaca
              , neighborConstraint drinkProp Rum originProp Karnaca
              , sideConstraint colorProp Purple isLeftOf colorProp Blue
              ]

solns :: [[Person]]
solns =
  solve constraints

main :: IO ()
main =
  do
    if length solns /= 1 then
      do
        displayMany solns
        printf "WARNING: %d solutions found.\n" (length solns)
    else
      display (head solns)

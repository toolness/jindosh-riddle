module Main exposing (main)

import Person exposing (..)
import Solver exposing (solve)
import Display exposing (displayList)
import Constraint exposing
  ( Constraint
  , simpleConstraint
  , neighborConstraint
  , sideConstraint
  )

constraints : List Constraint
constraints =
  [ simpleConstraint nameProp Contee colorProp Red
  , simpleConstraint positionProp farLeft nameProp Natsiou
  , simpleConstraint positionProp secondFromLeft colorProp Green
  , simpleConstraint positionProp center drinkProp Beer
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
  , sideConstraint colorProp Purple (<?) colorProp Blue
  ]

main =
  displayList (solve constraints)

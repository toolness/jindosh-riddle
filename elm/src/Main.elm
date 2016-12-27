module Main exposing (..)

import Html exposing (..)

import Solver exposing (..)
import Display exposing (display)
import Person exposing (..)
import Constraint exposing (..)

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

main : Html msg
main =
  let
    solns = solve constraints
    liDisplay soln =
      li [] [display soln]
  in
    ol [] (List.map liDisplay solns)

module Solver exposing (permutations, solve)

import Person exposing (..)
import Constraint exposing (Constraint, applyConstraints)

permutations : List x -> List (List x)
permutations list =
  let
    without : List x -> x -> List x
    without list item =
      List.filter (\x -> x /= item) list
    permutationsStartingWith : x -> List (List x)
    permutationsStartingWith x =
      List.map (\p -> x :: p) (permutations (without list x))
  in
    case list of
      [] -> []
      [a] -> [[a]]
      _ -> List.concat (List.map permutationsStartingWith list)

fillAbsentValues : Property x -> List Person -> List x -> List Person
fillAbsentValues prop people propValues =
  case propValues of
    [] -> people
    propValue::restPropValues ->
      case people of
        [] -> []
        person::restPeople ->
          case prop.get person of
            Nothing -> (prop.set person propValue)::(fillAbsentValues prop restPeople restPropValues)
            Just x -> person::(fillAbsentValues prop restPeople propValues)

-- This returns a list of potential solutions with any unknown
-- values along a given Property filled-in.
--
-- For instance, if three of five heirlooms have been filled-in, this
-- will return a list containing the two permutations of the solution
-- with all heirlooms filled-in.
permuteProperty : Property x -> List Person -> List (List Person)
permuteProperty prop people =
  let
    currentValues = List.map prop.get people
    isValueAbsent value =
      not (List.member (Just value) currentValues)
    valuesToPermute =
      List.filter isValueAbsent prop.values
  in
    List.map (fillAbsentValues prop people) (permutations valuesToPermute)

solveForProperty : List Constraint -> Property x -> List (List Person) -> List (List Person)
solveForProperty constraints prop candidates =
  let
    permutedCandidates = List.concat (List.map (permuteProperty prop) candidates)
  in
    List.filterMap (applyConstraints constraints) permutedCandidates

solve : List Constraint -> List (List Person)
solve constraints =
  let
    initialPeople =
      case applyConstraints constraints placedPeople of
        Nothing -> []
        Just x -> [x]
  in
    (solveForProperty constraints originProp
      (solveForProperty constraints drinkProp
        (solveForProperty constraints colorProp
          (solveForProperty constraints heirloomProp
            (solveForProperty constraints nameProp initialPeople)
          )
        )
      )
    )

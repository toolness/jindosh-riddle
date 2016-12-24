module Solver
  ( solve
  ) where

import Data.List
import Data.Maybe

import Person
import Constraint

fillAbsentValues :: Property x -> [Person] -> [x] -> [Person]
fillAbsentValues prop people propValues =
  if null people then []
    else
      let
        person = head people
      in
        case ((get prop) person) of
          Nothing -> ((set prop) person (head propValues)):fillAbsentValues prop (tail people) (tail propValues)
          Just x -> person:fillAbsentValues prop (tail people) propValues

-- This returns a list of potential solutions with any unknown
-- values along a given Property filled-in.
--
-- For instance, if three of five heirlooms have been filled-in, this
-- will return a list containing the two permutations of the solution
-- with all heirlooms filled-in.
permuteProperty :: (Eq x) => Property x -> [Person] -> [[Person]]
permuteProperty prop people =
  let
    getValue = get prop
    allValues = values prop
    currentValues = map getValue people
    isValueAbsent value =
      not ((Just value) `elem` currentValues)
    valuesToPermute =
      filter isValueAbsent allValues
  in
    map (fillAbsentValues prop people) (permutations valuesToPermute)

solveForProperty :: (Eq x) => [Constraint] -> Property x -> [[Person]] -> [[Person]]
solveForProperty constraints prop candidates =
  let
    permutedCandidates = concat (map (permuteProperty prop) candidates)
  in
    catMaybes (map (applyConstraints constraints) permutedCandidates)

solve :: [Constraint] -> [[Person]]
solve constraints =
  let
    placedPeople = map ((set positionProp) nullPerson) (values positionProp)
    initialPeople = (fromJust (applyConstraints constraints placedPeople))
  in
    (solveForProperty constraints originProp
      (solveForProperty constraints drinkProp
        (solveForProperty constraints colorProp
          (solveForProperty constraints heirloomProp
            (solveForProperty constraints nameProp [initialPeople])
          )
        )
      )
    )

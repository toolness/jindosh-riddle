module Constraint exposing 
  ( Constraint
  , simpleConstraint
  , applyConstraints
  )

import Person exposing (..)

-- A Constraint takes a potential solution and returns Nothing if the
-- solution violates the constraint. Alternatively, it will return the
-- potential solution, or an alteration of the potential solution based
-- on inferences made by the constraint logic.
type alias Constraint = List Person -> Maybe (List Person)

-- This returns a Constraint stating that a person with Property `aprop`
-- set to `a` must also have Property `bprop` set to `b`. It will
-- alter the proposed solution based on inferences made by this
-- constraint.
simpleConstraint : Property x -> x -> Property y -> y -> Constraint
simpleConstraint aprop a bprop b =
  let
    isConstraintViolated : Person -> Bool
    isConstraintViolated person =
      let
        oneWay aprop a bprop b =
          if aprop.get person == Just a &&
            bprop.get person /= Nothing &&
            bprop.get person /= Just b then True else False
      in
        oneWay aprop a bprop b || oneWay bprop b aprop a
    applyConstraint person =
      let
        oneWay aprop a bprop b person =
          if aprop.get person == Just a
            then bprop.set person b
            else person
      in
        oneWay aprop a bprop b person
          |> oneWay bprop b aprop a
    constraint people =
      if List.any isConstraintViolated people
        then Nothing
        else Just (List.map applyConstraint people)
  in
    constraint

-- Continuously apply Constraints to a potential solution until
-- a stable state is reached (that is, until none of the Constraints
-- return a changed solution, or until any Constraint is violated).
applyConstraints : List Constraint -> List Person -> Maybe (List Person)
applyConstraints constraints people =
  let      
    processAppliedConstraints newPeople =
      if newPeople == people
        -- Nothing changed when we applied the constraints, so
        -- just return them as-is.
        then Just people
        -- Something changed when applying the constraints, so
        -- keep applying them.
        else applyConstraints constraints newPeople
  in
    Maybe.andThen processAppliedConstraints
      (List.foldr Maybe.andThen (Just people) constraints)

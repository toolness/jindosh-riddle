module Constraint
  ( Constraint
  , simpleConstraint
  , neighborConstraint
  , sideConstraint
  , applyConstraints
  ) where

import Data.Maybe

import Person

-- A Constraint takes a potential solution and returns Nothing if the
-- solution violates the constraint. Alternatively, it will return the
-- potential solution, or an alteration of the potential solution based
-- on inferences made by the constraint logic.
type Constraint = [Person] -> Maybe [Person]

-- This returns a Constraint stating that a person with Property `aprop`
-- set to `a` must also have Property `bprop` set to `b`. It will
-- alter the proposed solution based on inferences made by this
-- constraint.
simpleConstraint :: (Eq x, Eq y) => Property x -> x -> Property y -> y -> Constraint
simpleConstraint aprop a bprop b =
  let
    isConstraintViolated person =
      let
        oneWay aprop a bprop b =
          if ((get aprop) person) == Just a &&
             ((get bprop) person) /= Nothing &&
             ((get bprop) person) /= Just b then True else False
      in
        if oneWay aprop a bprop b || oneWay bprop b aprop a
          then True else False
    applyConstraint person =
      let
        oneWay aprop a bprop b person =
          if ((get aprop) person) == Just a
            then ((set bprop) person b)
            else person
        in
          oneWay aprop a bprop b (oneWay bprop b aprop a person)
    constraint people =
      if any isConstraintViolated people
        then Nothing
        else Just (map applyConstraint people)
  in
    constraint

-- This returns a Constraint stating that a person with Property `aprop`
-- set to `a` must have a neighbor whose Property `bprop` is set to `b`.
neighborConstraint :: (Eq x, Eq y) => Property x -> x -> Property y -> y -> Constraint
neighborConstraint aprop a bprop b =
  let
    isViolatedByNeighbor person neighbor =
      let
        oneWay aprop a bprop b =
          if ((get aprop) person) == Just a &&
             ((get bprop) neighbor) /= Nothing &&
             ((get bprop) neighbor) /= Just b then True else False
      in
        oneWay aprop a bprop b || oneWay bprop b aprop a
    isViolatedByEitherNeighbor person neighbors =
      let
        oneWay aprop a bprop b =
          let
            bs = catMaybes (map (get bprop) neighbors)
          in
            if ((get aprop) person) == Just a &&
               length bs == 2 &&
               (not (b `elem` bs)) then True else False
      in
        oneWay aprop a bprop b || oneWay bprop b aprop a
    isViolatedByNeighbors person neighbors =
      if length neighbors == 1 then
        isViolatedByNeighbor person (head neighbors)
      else
        isViolatedByEitherNeighbor person neighbors
    isViolatedByAnyone people =
      any (\x -> isViolatedByNeighbors x (getNeighbors x people)) people
    constraint people =
      if isViolatedByAnyone people then Nothing else
        -- TODO: Consider inferring values based on this constraint
        -- to speed things up.
        Just people
  in
    constraint

-- This returns a Constraint stating that a person with Property `aprop`
-- set to `a` must be to the left or right of a person whose
-- Property `bprop` is set to `b`.
sideConstraint :: (Eq x, Eq y) => Property x -> x -> SideOperator -> Property y -> y -> Constraint
sideConstraint aprop a sideOp bprop b =
  let
    checkPosition personA personB defaultVal =
      let
        posA = (get positionProp) personA
        posB = (get positionProp) personB
      in
        if isNothing posA || isNothing posB then defaultVal else
          if personA `sideOp` (fromJust posB) then defaultVal else Nothing
    constraint people =
      let
        personA = findPerson aprop a people
        personB = findPerson bprop b people
      in
        if isNothing personA || isNothing personB then Just people else
          checkPosition (fromJust personA) (fromJust personB) (Just people)
  in
    constraint

-- Continuously apply Constraints to a potential solution until
-- a stable state is reached (that is, until none of the Constraints
-- return a changed solution, or until any Constraint is violated).
applyConstraints :: [Constraint] -> [Person] -> Maybe [Person]
applyConstraints constraints people =
  let
    applyConstraint constraint people =
      case people of
        Nothing -> Nothing
        Just x -> constraint x
    nextPeople =
      foldr applyConstraint (Just people) constraints
  in
    case nextPeople of
      Nothing -> Nothing
      Just x -> if x == people
        -- Nothing changed when we applied the constraints, so
        -- just return them as-is.
        then Just people
        -- Something changed when applying the constraints, so
        -- keep applying them.
        else applyConstraints constraints x

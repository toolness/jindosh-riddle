import Data.List
import Data.Maybe

data Name = Winslow | Marcolla | Contee | Natsiou | Finch
  deriving (Show, Eq, Enum)

data Color = Red | Green | Purple | Blue | White
  deriving (Show, Eq, Enum)

data Drink = Whiskey | Rum | Beer | Absinthe | Wine
  deriving (Show, Eq, Enum)

data Heirloom = Diamond | Tin | Pendant | Ring | Medal
  deriving (Show, Eq, Enum)

data Origin = Dunwall | Dabokva | Fraeport | Karnaca | Baleton
  deriving (Show, Eq, Enum)

data Position = FarLeft | SecondFromLeft | Center | SecondFromRight | FarRight
  deriving (Show, Eq, Enum)

data Person = Person { name :: Maybe Name
                     , color :: Maybe Color
                     , drink :: Maybe Drink
                     , heirloom :: Maybe Heirloom
                     , origin :: Maybe Origin
                     , position :: Maybe Position }
  deriving (Show, Eq)

nullPerson = Person { name=Nothing
                    , color=Nothing
                    , drink=Nothing
                    , heirloom=Nothing
                    , origin=Nothing
                    , position=Nothing }

allValues :: (Enum a) => [a]
allValues = (enumFrom (toEnum 0))

data Property x =Property { get :: Person -> Maybe x
                          , set :: Person -> x -> Person
                          , values :: [x] }

nameProp = Property { get=name
                    , set=(\p x -> p { name=Just x })
                    , values=allValues }

colorProp = Property { get=color
                     , set=(\p x -> p { color=Just x })
                     , values=allValues}

drinkProp = Property { get=drink
                     , set=(\p x -> p { drink=Just x })
                     , values=allValues }

heirloomProp = Property { get=heirloom
                        , set=(\p x -> p { heirloom=Just x })
                        , values=allValues }

originProp = Property { get=origin
                      , set=(\p x -> p { origin=Just x })
                      , values=allValues }

positionProp = Property { get=position
                        , set=(\p x -> p { position=Just x })
                        , values=allValues }

type Constraint = [Person] -> Maybe [Person]

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

constraints :: [Constraint]
constraints = [ simpleConstraint nameProp Contee colorProp Red
              , simpleConstraint positionProp FarLeft nameProp Natsiou
              , simpleConstraint positionProp SecondFromLeft colorProp Green
              , simpleConstraint positionProp Center drinkProp Beer
              , simpleConstraint drinkProp Wine colorProp Purple
              , simpleConstraint originProp Dabokva colorProp White ]

people :: [Person]
people = map ((set nameProp) nullPerson) (values nameProp)

-- TODO: Continuously permute people and apply constraints until a
-- solution is found.

soln = applyConstraints constraints people

solnWithPermutedColors = permuteProperty colorProp (fromJust soln)

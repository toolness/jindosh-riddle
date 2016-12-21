data Name = Winslow | Marcolla | Contee | Natsiou | Finch
  deriving (Show, Eq, Enum)

data Color = Red | Green | Purple | Blue | White
  deriving (Show, Eq)

data Drink = Whiskey | Rum | Beer | Absinthe | Wine
  deriving (Show, Eq)

data Heirloom = Diamond | Tin | Pendant | Ring | Medal
  deriving (Show, Eq)

data Origin = Dunwall | Dabokva | Fraeport | Karnaca | Baleton
  deriving (Show, Eq)

data Position = FarLeft | SecondFromLeft | Center | SecondFromRight | FarRight
  deriving (Show, Eq)

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

allNames :: [Name]
allNames = (enumFrom (toEnum 0 :: Name))

data Property x =Property { get :: Person -> Maybe x
                          , set :: Person -> x -> Person }

nameProp = Property { get=name, set=(\p x -> p { name=Just x }) }
colorProp = Property { get=color, set=(\p x -> p { color=Just x }) }
drinkProp = Property { get=drink, set=(\p x -> p { drink=Just x }) }
heirloomProp = Property { get=heirloom, set=(\p x -> p { heirloom=Just x }) }
originProp = Property { get=origin, set=(\p x -> p { origin=Just x }) }
positionProp = Property { get=position, set=(\p x -> p { position=Just x }) }

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

applyConstraints constraints people =
  let
    applyConstraint constraint people =
      case people of
        Nothing -> Nothing
        Just x -> constraint x
  in
    foldr applyConstraint (Just people) constraints

constraints :: [Constraint]
constraints = [ simpleConstraint nameProp Contee colorProp Red ]

people :: [Person]
people = map ((set nameProp) nullPerson) allNames

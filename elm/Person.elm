module Person exposing (..)

type Name = Winslow | Marcolla | Contee | Natsiou | Finch
type Color = Red | Green | Purple | Blue | White
type Drink = Whiskey | Rum | Beer | Absinthe | Wine
type Heirloom = Diamond | Tin | Pendant | Ring | Medal
type Origin = Dunwall | Dabokva | Fraeport | Karnaca | Baleton

-- Elm doesn't allow union types to be compared in an ordinal way, so
-- we'll just use integers to represent positions.
type alias Position = Int

farLeft = 0
secondFromLeft = 1
center = 2
secondFromRight = 3
farRight = 4

-- A Person represents a person in the Jindosh riddle whose various
-- properties may still be unknown or have a concrete value.
type alias Person =
  { name: Maybe Name
  , color: Maybe Color
  , drink: Maybe Drink
  , heirloom: Maybe Heirloom
  , origin: Maybe Origin
  , position: Maybe Position
  }

nullPerson : Person
nullPerson = 
  { name=Nothing
  , color=Nothing
  , drink=Nothing
  , heirloom=Nothing
  , origin=Nothing
  , position=Nothing
  }

-- A Property represents metadata about a particular aspect of a
-- person.
type alias Property x = 
  { get : Person -> Maybe x
  , set : Person -> x -> Person
  , values : List x
  }

nameProp : Property Name
nameProp =
  { get=.name
  , set=(\p -> \x -> {p | name=Just x})
  , values=[Winslow, Marcolla, Contee, Natsiou, Finch]
  }

colorProp : Property Color
colorProp =
  { get=.color
  , set=(\p -> \x -> {p | color=Just x})
  , values=[Red, Green, Purple, Blue, White]
  }

drinkProp : Property Drink
drinkProp =
  { get=.drink
  , set=(\p -> \x -> {p | drink=Just x})
  , values=[Whiskey, Rum, Beer, Absinthe, Wine]
  }

heirloomProp : Property Heirloom
heirloomProp =
  { get=.heirloom
  , set=(\p -> \x -> {p | heirloom=Just x})
  , values=[Diamond, Tin, Pendant, Ring, Medal]
  }

originProp : Property Origin
originProp =
  { get=.origin
  , set=(\p -> \x -> {p | origin=Just x})
  , values=[Dunwall, Dabokva, Fraeport, Karnaca, Baleton]
  }

positionProp : Property Position
positionProp =
  { get=.position
  , set=(\p -> \x -> {p | position=Just x})
  , values=[farLeft, secondFromLeft, center, secondFromRight, farRight]
  }

leftOf : Position -> Position
leftOf pos = pos - 1

rightOf : Position -> Position
rightOf pos = pos + 1

-- A SideOperator is just an infix operator that tells us whether
-- a person is to the left or right of a particular Position.
type alias SideOperator = Person -> Position -> Bool

(<?) : SideOperator
(<?) person position =
  if position == farLeft then False else
    person.position == Just (leftOf position)

(>?) : SideOperator
(>?) person position =
  if position == farRight then False else
    person.position == Just (rightOf position)

getNeighbors : Person -> List Person -> List Person
getNeighbors person people =
  let
    isNeighbor : Position -> Person -> Bool
    isNeighbor position person =
      person <? position || person >? position
  in
    case person.position of
      Nothing -> []
      Just x -> List.filter (isNeighbor x) people

placedPeople : List Person
placedPeople =
  List.map (positionProp.set nullPerson) positionProp.values

findPerson : Property x -> x -> List Person -> Maybe Person
findPerson prop value people =
  case people of
    [] -> Nothing
    person::everyoneElse ->
      if prop.get person == Just value
        then Just person
        else findPerson prop value everyoneElse

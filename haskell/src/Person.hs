module Person where

-- Here we define the various aspects of an individual's persona as
-- separate types. This may be unnecessary, but after reading
-- the book's chapters on types and typeclasses, I thought it'd
-- be nice to get more hands-on experience with types, and also
-- useful for the compiler to ensure that I hadn't made certain
-- kinds of mistakes.
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
  deriving (Show, Eq, Enum, Ord)

-- A Person represents a person in the Jindosh riddle whose various
-- properties may still be unknown or have a concrete value.
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

-- Return a lost of all possible values of an Enum. Haskell's type
-- inference will figure out which Enum we want the values of.
allValues :: (Enum a) => [a]
allValues = (enumFrom (toEnum 0))

-- A Property represents metadata about a particular aspect of a
-- person.
--
-- It seems like Haskell has a concept called "Lenses" which might
-- do something similar, but its documentation was confusing so I
-- went this route instead.
data Property x = Property { get :: Person -> Maybe x
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

leftOf :: Position -> Position
leftOf position = pred position

rightOf :: Position -> Position
rightOf position = succ position

-- A SideOperator is just an infix operator that tells us whether
-- a person is to the left or right of a particular Position.
type SideOperator = Person -> Position -> Bool

isLeftOf :: SideOperator
isLeftOf person position =
  if position == FarLeft then False else
    (get positionProp) person == Just (leftOf position)

isRightOf :: SideOperator
isRightOf person position =
  if position == FarRight then False else
    (get positionProp) person == Just (rightOf position)

getNeighbors :: Person -> [Person] -> [Person]
getNeighbors person people =
  let
    getPosition x = (get positionProp) x
    isNeighbor position person =
      person `isLeftOf` position || person `isRightOf` position
  in
    case getPosition person of
      Nothing -> []
      Just x -> filter (isNeighbor x) people

findPerson :: (Eq x) => Property x -> x -> [Person] -> Maybe Person
findPerson prop value people =
  if null people
    then Nothing
  else
    let
      person = head people
    in
      if ((get prop) person) == Just value
        then Just person
        else findPerson prop value (tail people)

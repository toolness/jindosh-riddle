data Name = Winslow | Marcolla | Contee | Natsiou | Finsh
  deriving (Show, Eq)

data Color = Red | Green | Purple | Blue | White
  deriving (Show, Eq)

data Drink = Whiskey | Rum | Beer | Absinthe | Wine
  deriving (Show, Eq)

data Heirloom = Diamond | Tin | Pendant | Ring | Medal
  deriving (Show, Eq)

data Origin = Dunwall | Dabokva | Fraeport | Karnaca | Baleton
  deriving (Show, Eq)

type Position = Integer

data Person = Person {
  name :: Name
, color :: Color
, drink :: Drink
, heirloom :: Heirloom
, origin :: Origin
, position :: Position
} deriving (Show, Eq)

p = Person { name=Winslow, color=Red, drink=Whiskey, heirloom=Diamond, origin=Dunwall, position=1 }

boop = \a -> if drink a == Wine then color a == Purple else True

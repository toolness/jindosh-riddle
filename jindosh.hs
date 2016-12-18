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

data Person = Person {
  name :: Maybe Name
, color :: Maybe Color
, drink :: Maybe Drink
, heirloom :: Maybe Heirloom
, origin :: Maybe Origin
, position :: Maybe Position
} deriving (Show, Eq)

person = Person {
  name = Nothing
, color = Nothing
, drink = Nothing
, heirloom = Nothing
, origin = Nothing
, position = Nothing
}

simpleRules =
  [ person { name=Just Contee, color=Just Red }
  , person { position=Just FarLeft, name=Just Natsiou }
  , person { position=Just SecondFromLeft, color=Just Green }
  , person { position=Just Center, drink=Just Beer }
  , person { drink=Just Wine, color=Just Purple }
  , person { origin=Just Dabokva, color=Just White }
  , person { name=Just Winslow, heirloom=Just Diamond }
  , person { origin=Just Baleton, heirloom=Just Ring }
  , person { name=Just Finch, drink=Just Absinthe }
  , person { origin=Just Dunwall, drink=Just Whiskey }
  , person { name=Just Marcolla, origin=Just Fraeport } ]

-- "the lady in purple sat left of someone in blue"
-- (so the lady in purple is NOT on the far right seat)

-- "when one of the dinner guests bragged about her Snuff Tin,
--  the woman next to her said they were finer in Dabokva, where she lived"
-- (does this mean the snuff tin person has only one neighbor?)

-- "someone else carried a valuable war medal, and when she saw it the
--  visitor from karnaca next to her almost spilled her neighbor's rum"
-- (does this mean the karnacan has two neighbors?)

-- the lady from Dunwall fell onto the guest in the center seat
-- (so the center seat is NOT from dunwall)

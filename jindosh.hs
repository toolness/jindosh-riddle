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

allNames = (enumFrom (toEnum 0 :: Name))

people = map
  (\name -> Person { name=Just name
                   , color=Nothing
                   , drink=Nothing
                   , heirloom=Nothing
                   , origin=Nothing
                   , position=Nothing })
  allNames

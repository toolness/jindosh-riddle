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

data Property = Name Name | Color Color | Drink Drink | Heirloom Heirloom |
  Origin Origin | Position Position
  deriving (Show, Eq)

rule :: Property -> Property -> [(Property, Property)]

rule a b =
  [(a, b), (b, a)]

rulebook rules = rules

rules = rulebook (
  rule (Name Contee) (Color Red) ++
  rule (Position FarLeft) (Name Natsiou) ++
  rule (Position SecondFromLeft) (Color Green) ++
  rule (Position Center) (Drink Beer) ++
  rule (Drink Wine) (Color Purple) ++
  rule (Origin Dabokva) (Color White) ++
  rule (Name Winslow) (Heirloom Diamond) ++
  rule (Origin Baleton) (Heirloom Ring) ++
  rule (Name Finch) (Drink Absinthe) ++
  rule (Origin Dunwall) (Drink Whiskey) ++
  rule (Name Marcolla) (Origin Fraeport)
  )

-- "the lady in purple sat left of someone in blue"
-- (so the lady in purple is NOT on the far right seat.
    also not in second from left, or far left. second from right is wearing white, so she can't be in center. )

-- "when one of the dinner guests bragged about her Snuff Tin,
--  the woman next to her said they were finer in Dabokva, where she lived"
-- (does this mean the snuff tin person has only one neighbor?)
--   (if so, it's not the far left, b/c then the second-from-left would
--    be wearing both green AND white which is impossible, so it's far right.
--    this means second from right is from dabovka and wearing white.)

-- "someone else carried a valuable war medal, and when she saw it the
--  visitor from karnaca next to her almost spilled her neighbor's rum"
-- (does this mean the karnacan has two neighbors?)
--   (if so, the karnacan is either at center or second from left, since
--    the lady from dabovka is second from right.)

--    if the karnacan is second from left: she is wearing green

-- the lady from Dunwall fell onto the guest in the center seat
-- (so the center seat is NOT from dunwall)

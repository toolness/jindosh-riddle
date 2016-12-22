import Data.List
import Data.Maybe
import Text.Printf

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

leftOf :: Position -> Position
leftOf position = pred position

rightOf :: Position -> Position
rightOf position = succ position

isLeftOf :: Position -> Person -> Bool
isLeftOf position person =
  if position == FarLeft then False else
    (get positionProp) person == Just (leftOf position)

isRightOf :: Position -> Person -> Bool
isRightOf position person =
  if position == FarRight then False else
    (get positionProp) person == Just (rightOf position)

getNeighbors :: Person -> [Person] -> [Person]
getNeighbors person people =
  let
    getPosition x = (get positionProp) x
    isNeighbor position person =
      isLeftOf position person || isRightOf position person
  in
    case getPosition person of
      Nothing -> []
      Just x -> filter (isNeighbor x) people

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
        -- TODO: Consider deducing values based on this constraint
        -- to speed things up.
        Just people
  in
    constraint

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

leftOfConstraint :: (Eq x, Eq y) => Property x -> x -> Property y -> y -> Constraint
leftOfConstraint aprop a bprop b =
  let
    checkPosition personA personB defaultVal =
      let
        posA = (get positionProp) personA
        posB = (get positionProp) personB
      in
        if isNothing posA || isNothing posB then defaultVal else
          if isLeftOf (fromJust posB) personA then defaultVal else Nothing
    constraint people =
      let
        personA = findPerson aprop a people
        personB = findPerson bprop b people
      in
        if isNothing personA || isNothing personB then Just people else
          checkPosition (fromJust personA) (fromJust personB) (Just people)
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

solveForProperty :: (Eq x) => [Constraint] -> Property x -> [[Person]] -> [[Person]]
solveForProperty constraints prop candidates =
  let
    permutedCandidates = concat (map (permuteProperty prop) candidates)
  in
    catMaybes (map (applyConstraints constraints) permutedCandidates)

constraints :: [Constraint]
constraints = [ simpleConstraint nameProp Contee colorProp Red
              , simpleConstraint positionProp FarLeft nameProp Natsiou
              , simpleConstraint positionProp SecondFromLeft colorProp Green
              , simpleConstraint positionProp Center drinkProp Beer
              , simpleConstraint drinkProp Wine colorProp Purple
              , simpleConstraint originProp Dabokva colorProp White
              , simpleConstraint nameProp Winslow heirloomProp Diamond
              , simpleConstraint originProp Baleton heirloomProp Ring
              , simpleConstraint nameProp Finch drinkProp Absinthe
              , simpleConstraint originProp Dunwall drinkProp Whiskey
              , simpleConstraint nameProp Marcolla originProp Fraeport
              , neighborConstraint heirloomProp Tin originProp Dabokva
              , neighborConstraint heirloomProp Medal originProp Karnaca
              , neighborConstraint drinkProp Rum originProp Karnaca
              , leftOfConstraint colorProp Purple colorProp Blue
              ]

initialPeople :: [Person]
initialPeople =
  let
    placedPeople = map ((set positionProp) nullPerson) (values positionProp)
  in
    (fromJust (applyConstraints constraints placedPeople))

solns :: [[Person]]
solns =
  (solveForProperty constraints originProp
    (solveForProperty constraints drinkProp
      (solveForProperty constraints colorProp
        (solveForProperty constraints heirloomProp
          (solveForProperty constraints nameProp [initialPeople])
        )
      )
    )
  )

soln :: [Person]
soln = head solns

display :: [Person] -> IO ()
display people =
  let
    getPropValue prop person =
      let
        value = ((get prop) person)
      in
        if value == Nothing then "??" else show (fromJust value)
    displayProp :: (Show x, Eq x) => Property x -> Person -> IO ()
    displayProp prop person =
      do
        printf "%20s " (getPropValue prop person)
    displayRow prop people =
      if null people then
        printf "\n"
        else
          do
            displayProp prop (head people)
            displayRow prop (tail people)
  in
    do
      displayRow nameProp people
      displayRow heirloomProp people
      displayRow drinkProp people
      displayRow originProp people
      displayRow colorProp people
      displayRow positionProp people

displayMany :: [[Person]] -> IO ()
displayMany candidates =
  if null candidates then return () else
    do
      display (head candidates)
      printf "\n"
      displayMany (tail candidates)

main =
  do
    if length solns /= 1 then
      do
        displayMany solns
        printf "WARNING: %d solutions found.\n" (length solns)
    else
      display soln

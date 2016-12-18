isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = reverse a == a

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else -x

-- f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f x y = ((snd x, snd y), (fst x, fst y))

-- x = (+)

-- f xs = w `x` 1
--   where w = length xs

f (a, b) = a

type Buttsack = [Int]

bop :: Buttsack -> Int
bop x = head x

zoot :: [a] -> a
zoot (x:xs) = x

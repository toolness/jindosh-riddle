boop :: Num a => a -> a -> a
boop x y = x + 3 * y

blurp :: Num a => a -> a
blurp = boop 1

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

boop = x * y
  where x = 2
        y = 4

one = x * 3 + y
  where x = 3
        y = 1000

two = x * 5
  where x = 10 * 5 + y
        y = 10

three = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple x = x * 3

waxOff x = triple x

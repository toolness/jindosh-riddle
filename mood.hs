data Mood = Blah | Awesome deriving Show

swing :: Mood -> Mood

swing Blah = Awesome
swing _ = Blah

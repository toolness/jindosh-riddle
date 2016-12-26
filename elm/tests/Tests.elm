module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import Solver exposing (permutations)

all : Test
all =
    describe "Jindosh Test Suite"
        [ describe "permutations"
            [ test "it works" <|
                \() ->
                    Expect.equal (permutations [1,2])  [[1,2], [2,1]]
            ]
        ]

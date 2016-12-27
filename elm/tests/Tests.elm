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
            [ test "it works w/ [1,2]" <|
                \() ->
                    Expect.equal (permutations [1,2])  [[1,2], [2,1]]
            , test "it works w/ [1,2,3]" <|
                \() ->
                    Expect.equal (permutations [1,2,3])
                    [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
            , test "it works w/ [1,2,3,4]" <|
                \() ->
                    Expect.equal (List.length (permutations [1,2,3,4]))
                    (4 * 3 * 2 * 1)
            ]
        ]

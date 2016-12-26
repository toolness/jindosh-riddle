module Solver exposing (..)

permutations : List x -> List (List x)
permutations list =
  let
    without : List x -> x -> List x
    without list item =
      List.filter (\x -> x /= item) list
  in
    List.map (\x -> x :: (without list x)) list

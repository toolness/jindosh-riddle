module Display exposing (display)

import Html exposing (table, tr, td, text, Html)
import Html.Attributes exposing (style)

import Person exposing (..)

display : List Person -> Html msg
display people =
  let
    cell : Maybe x -> Html msg
    cell value =
      let
        css = [
          ("font-family", "monospace"),
          ("min-width", "10em")
        ]
        unknownCss = css ++ [
          ("color", "gray")
        ]
      in
        case value of
          Nothing -> td [style unknownCss] [text "??"]
          Just x -> td [style css] [text (toString x)]

    row : List Person -> (Person -> Maybe x) -> Html msg
    row people getter =
      tr [] (List.map cell (List.map getter people))
  in
    table [] [
      row people .name,
      row people .heirloom,
      row people .drink,
      row people .origin,
      row people .color,
      row people .position
    ]

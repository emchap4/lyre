module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)

view model = 
    div [ class "jumbotron" ]
        [ h1 [] [ text "Here is some LARGE TEXT" ]
        , p []
            [ text "Bottom text"
            , strong [] [ text "Cooler text" ]
            ]
        ]

main =
    view "Example model"

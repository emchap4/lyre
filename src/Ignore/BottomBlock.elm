module BottomBlock exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Browser





main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model =
    { 

    }


init : () -> (Model, Cmd Msg)
init _ =
    (Model modelInitialValue, Cmd.none)


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msg1 ->
            (model, Cmd.none)

        Msg2 ->
            (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ text "New Element" ]



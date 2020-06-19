module FallingBlock exposing (..)

--import List exposing (List, (::))
import String
import Tuple 

import Platform.Cmd as Cmd

import Svg
import Svg.Attributes as SvgA

import Browser
import Browser.Events

import Html exposing (..)


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }



-- MODEL

type alias Model =
    { position : (Float, Float)
    , size : (Float, Float)
    , roundness : Float
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( Model (16, 0) (256, 128) 15
    , Cmd.none)



-- UPDATE


type Msg
    = OnAnimationFrame Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OnAnimationFrame _ ->
            ( { model | position = (Tuple.first model.position, Tuple.second model.position + 1) }
            , Cmd.none
            )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta OnAnimationFrame



-- VIEW

view : Model -> Html Msg
view model =
    let 
        sfs = String.fromFloat (Tuple.first model.size)
        sss = String.fromFloat (Tuple.second model.size * 4)
    in
    Svg.svg
    [ SvgA.width sfs
    , SvgA.height sss
    , SvgA.viewBox ("0 0 " ++ String.fromFloat (Tuple.first model.size +  Tuple.first model.position) ++ " " ++ sss)
    ]
    [viewBlock model]


viewBlock : Model -> Html Msg
viewBlock model =
    Svg.rect
        [ SvgA.x (String.fromFloat (Tuple.first model.position))
        , SvgA.y (String.fromFloat (Tuple.second model.position))
        , SvgA.width (String.fromFloat (Tuple.first model.size))
        , SvgA.height (String.fromFloat (Tuple.second model.size))
        , SvgA.rx (String.fromFloat model.roundness)
        , SvgA.ry (String.fromFloat model.roundness)
        ]
        []


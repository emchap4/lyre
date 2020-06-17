module Block exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Events
import Helpers.Grid as Grid
import Html exposing (..)
import Svg
import Svg.Attributes as SvgA


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type BlockType
    = Falling FallingBlockInfo
    | Bottom BottomBlockInfo


type alias FallingBlockInfo =
    { blockGridSize : Int
    , blockSize : ( Float, Float )
    , fallSpeed : Float
    }


type alias BottomBlockInfo =
    { active : Bool
    , blockSize : ( Float, Float )
    }


type Word
    = Noun NounInfo
    | Verb VerbInfo


type alias NounInfo =
    { name : String
    , declension : String
    }


type alias VerbInfo =
    { name : String
    , tense : String
    }


type alias Model =
    { blockType : BlockType
    , gridPosition : Grid.GridPosition
    , word : Maybe Word
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Falling
            { blockGridSize = 1
            , blockSize = ( 256, 128 )
            , fallSpeed = 1
            }
        )
        { x = 0, y = 0 }
        Nothing
    , Cmd.none
    )

{--
            { active = False
            , blockSize = ( 256, 128 )
            }
--}

{--
(Falling
            { blockGridSize = 1
            , blockSize = ( 256, 128 )
            , fallSpeed = 1
            }
        )

--}

type Msg
    = OnAnimationFrame Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame _ ->
            case model.blockType of
                Falling _->
                    ( { model | gridPosition = 
                        { x = model.gridPosition.x
                        , y = model.gridPosition.y + round (getBlockFallSpeed model.blockType)
                        } }
                    , Cmd.none)
                Bottom _->
                    ( model
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta OnAnimationFrame


view : Model -> Html Msg
view model =
    let
        xS =
            Tuple.first (getBlockSize model.blockType)

        yS =
            Tuple.second (getBlockSize model.blockType) * 4
    in
    Svg.svg
        [ SvgA.width <| String.fromFloat <| xS
        , SvgA.height <| String.fromFloat <| yS
        , SvgA.viewBox ("0 0 " ++ String.fromFloat (xS + toFloat model.gridPosition.x) ++ " " ++ String.fromFloat yS)
        ]
        [ viewBlock model ]


viewBlock : Model -> Html Msg
viewBlock model =
    Svg.rect
        [ SvgA.x (String.fromInt model.gridPosition.x)
        , SvgA.y (String.fromInt model.gridPosition.y)
        , SvgA.width (String.fromFloat (Tuple.first (getBlockSize model.blockType)))
        , SvgA.height (String.fromFloat (Tuple.second (getBlockSize model.blockType)))
        , SvgA.rx "15"
        , SvgA.ry "15"
        ]
        []


getBlockSize : BlockType -> ( Float, Float )
getBlockSize blockType =
    case blockType of
        Bottom bottomBlockInfo ->
            bottomBlockInfo.blockSize

        Falling fallingBlockInfo ->
            fallingBlockInfo.blockSize

getBlockFallSpeed : BlockType -> Float
getBlockFallSpeed fallingBlockType =
    case fallingBlockType of 
        Falling fallingBlockInfo ->
            fallingBlockInfo.fallSpeed
        
        Bottom bottomBlockInfo ->
            0


--getBottomBlockInfo (Bottom i) = i
--getFallingBlockInfo (Falling i) = i
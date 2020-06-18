module Main exposing (main)

import Html exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Task
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Random
import Array
--import Collage
--import Element
import Svg.Attributes exposing (x)


allWordList : List Word
allWordList = 
    [ Noun "test1" "test1"
    , Verb "test2" "test2"
    , Noun "test3" "test3"

    ]






-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


-- INIT

init : () -> (Model, Cmd Msg)
init _ =
    ( defaultModel, getViewportCommand )

getViewportCommand : Cmd Msg
getViewportCommand = 
    Task.perform ViewportSize Browser.Dom.getViewport


-- MODEL

modelCfg =
    { gameWidth = 600
    , gameHeight = 400
    , halfWidth = 300.0
    , halfHeight = 200.0
    , blockWidth = 220
    , blockHeight = 35
    , blockCornerRounding = 5
    , blockDistX = 240
    , blockDistY = 55
    , blockCols = 5
    , blockRows = 3
    , blocksDistFromTopY = 420
    }

type alias Positioned a =
    {a | x : Float, y : Float}

type alias GridPositioned a =
    {a | gridX : Int, gridY : Int }

type alias Moving a =
    { a | vy : Float }

type alias Sized a =
    { a | w : Float, h : Float }

type alias Colored a =
    { a | color : String}

type alias Labeled a =
    { a | word : Word}

type alias Block =
    Labeled(Colored(Sized(GridPositioned((Positioned {})))))

type alias FallingBlock =
    Moving (Block)

-- Word on a fallingBlock or bottomBlock
type Word
    -- Noun name declension
    = Noun String String
    -- Verb name tense
    | Verb String String

type ColumnState 
    = AllActive
    --SomeUnactive, How many are unactive from top 
    | SomeUnactive Int

type State 
    = Play
    | Lost



block : Float -> Float -> Int -> Int -> Float -> Float -> Word -> String -> Block
block x y gx gy w h word color =
    { x = x, y = y, gridX = gx, gridY = gy, w = w, h = h, word = word, color = color}

blockRow : Float -> Float -> List Block
blockRow viewportX y=
    let
        xOff =
            toFloat (-modelCfg.blockCols // 2 |> toFloat |> ceiling)
                * modelCfg.blockDistX + viewportX - modelCfg.blockWidth / 2
    in
        List.map
            (\x ->
                block (modelCfg.blockDistX * x + xOff)
                    y
                    0 0
                    modelCfg.blockWidth
                    modelCfg.blockHeight
                    (Noun "" "")
                    "purple"
            ) ((List.range 0 (modelCfg.blockCols - 1)) |> List.map toFloat)

add : number -> number -> number
add a b =
    a + b

type alias Model =
    { state : State
    , blocks : List Block
    --, fallingBlocks : Maybe (List FallingBlock)
    , columnStates : List ColumnState
    , windowDimensions : Browser.Dom.Viewport
    }

defaultModel : Model
defaultModel =
    { state = Play
    , blocks = List.map (((*) modelCfg.blockDistY))
            ((List.range 0 (modelCfg.blockRows - 1)) |> List.map toFloat) |> List.map (add modelCfg.blocksDistFromTopY)
            |> List.map (blockRow 640)
            |> List.concat
    --, fallingBlocks = Nothing
    , columnStates = List.repeat modelCfg.blockCols AllActive
    , windowDimensions =    { scene = 
                                { width = 640, height = 480 }
                            , viewport = 
                                { x = 0, y = 0, width = 640, height = 480 } 
                            }
    }


-- UPDATE

type Msg
    = NoOp
    | ViewportSize Browser.Dom.Viewport
    | OnAnimationFrame Float
    | FindRandomWord Block
    | RandomWord (Int, Block)
    | ColorIn Block


--generatorWordIndexBlock : Block -> Random.Generator (Int, Block)
--generatorWordIndexBlock b =
--    (Random.int 0 (List.length allWordList - 1), b)

randomWordListIndex : Block -> Random.Generator (Int, Block)
randomWordListIndex b =
    Random.pair (Random.int 0 (List.length allWordList - 1)) (Random.constant b)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

        ViewportSize newSize ->
            ( { model | windowDimensions = newSize}, Cmd.none)

        OnAnimationFrame dt ->
            (model, Cmd.none)
        
        ColorIn b ->
            ( { model | blocks =
                List.map 
                    ( \x -> 
                        if x.x == b.x && x.y == b.y then
                            { x | color = "green" }
                        else
                            x
                    )
                    model.blocks
                }
            , Cmd.none
            )
        
        FindRandomWord b ->
            ( model
            , Random.generate RandomWord (randomWordListIndex b)
            )

        RandomWord (rn, b) ->
            let 
                selectedWord = Array.fromList allWordList
                    |> Array.get rn
            in
                ( { model | blocks =
                    List.map 
                        ( \x -> 
                            if x.x == b.x && x.y == b.y then
                                case selectedWord of
                                    Just word ->
                                        { x | word = word }--Noun "test3" "test" }
                                    Nothing ->
                                        x
                            else
                                x
                        )
                        model.blocks
                    }
                , Cmd.none
                )

        

-- VIEW

view : Model -> Html Msg
view model =
    displayBlocks model


displayFullScreen : Browser.Dom.Viewport -> List (Html Msg) -> Html Msg
displayFullScreen v content =
    let
        -- to prevent scrolling down when user hits space bar
        height_ =
            v.viewport.height - 30

        width =
            v.viewport.width

        gameScale =
            Basics.min (width / modelCfg.gameWidth)
                (height_ / modelCfg.gameHeight)
    in
        Svg.svg
            [ SvgA.viewBox "0 0 1275 570"--("0 0" ++ String.fromFloat width ++ " " ++ String.fromFloat height_)--"0 0 640 480"--("0 0 " ++ String.fromInt modelCfg.gameWidth ++ " " ++ String.fromInt modelCfg.gameHeight)
            , SvgA.preserveAspectRatio "xMidYMax"
            , SvgA.width (String.fromFloat width)
            , SvgA.height (String.fromFloat height_)
            ]
            content

displayBlocks : Model -> Html Msg
displayBlocks ({ blocks } as model) = 
    let 
        blockRects =
            List.map (\b -> 
                        Svg.g
                            []
                            [ Svg.rect   
                                [ SvgA.width (String.fromFloat b.w)
                                , SvgA.height (String.fromFloat b.h)
                                , SvgA.x (String.fromFloat b.x)
                                , SvgA.y (String.fromFloat b.y)
                                , SvgA.rx (String.fromFloat modelCfg.blockCornerRounding)
                                , SvgA.fill b.color
                                , SvgE.onClick (FindRandomWord b) --SvgE.onClick (ColorIn b)
                                ]
                                []
                            , Svg.text_
                                [ SvgA.x (String.fromFloat (b.x + 20))
                                , SvgA.y (String.fromFloat (b.y + 26))
                                , SvgA.fontSize "30"
                                , SvgA.fill "white"
                                ]
                                [Svg.text ( getWordName b.word)]
                            
                            ]
                        ) blocks
    in
        displayFullScreen model.windowDimensions blockRects


getWordName : Word -> String
getWordName word =
    case word of
        Noun name _ ->
            name
        Verb name _ ->
            name


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Browser.Events.onResize (\w h -> 
            ViewportSize { scene = { width = toFloat w, height = toFloat h} 
            , viewport = { x = 0, y = 0, width = toFloat w, height = toFloat h}
            })
        ]

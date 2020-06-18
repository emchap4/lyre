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
import Update.Extra


allWordList : List Word
allWordList = 
    [ Noun "you" "test1"
    , Verb "bean" "test2"
    , Noun "man" "test3"
    , Noun "never" ""
    , Noun "gonna" ""
    , Noun "give" ""
    , Noun "up" ""
    ]

filterAllWordList : List Word -> Bool -> List Word -> List Word
filterAllWordList initialwl allowRepeats blacklistwl =
    List.filter (isNotInBlacklistedWords blacklistwl) initialwl


isNotInBlacklistedWords : List Word -> Word -> Bool
isNotInBlacklistedWords blacklistwl w =
    not (List.member w blacklistwl)

            {--let 
                fws = filterAllWordList xs
            in
                if x == fws then
                    x
                else
                    fws--}


filteredWords : List Word
filteredWords =
    []


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
    ( defaultModel
    , Cmd.batch 
        [ getViewportCommand
        , Cmd.batch (List.map (\b -> Random.generate RandomWord (randomWordListIndex b)) 
                        (List.map (((*) modelCfg.blockDistY))
                        ((List.range 0 (modelCfg.blockRows - 1)) |> List.map toFloat) 
                        |> List.map (add modelCfg.blocksDistFromTopY)
                        |> List.map (blockRow 640)
                        |> List.concat)
            )
        ]
    )

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
    , fallingBlockSpawnHeight = -500
    , fallingBlockSpeed = -1
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

fallingBlock : Float -> Float -> Float -> Int -> Int -> Float -> Float -> Word -> String -> FallingBlock
fallingBlock x y vy gx gy w h word color =
    { x = x, y = y, gridX = gx, gridY = gy, w = w, h = h, word = word, color = color, vy = vy}

fallingBlockRow : Float -> List FallingBlock
fallingBlockRow viewportX = 
    let
        xOff =
            toFloat (-modelCfg.blockCols // 2 |> toFloat |> ceiling)
                * modelCfg.blockDistX + viewportX - modelCfg.blockWidth / 2
    in
        List.map
            (\x ->
                fallingBlock (modelCfg.blockDistX * x + xOff)
                    modelCfg.fallingBlockSpawnHeight
                    modelCfg.fallingBlockSpeed
                    (round x)
                    (round modelCfg.fallingBlockSpawnHeight)
                    modelCfg.blockWidth
                    modelCfg.blockHeight
                    (Noun "" "")
                    "purple"
            ) ((List.range 0 (modelCfg.blockCols - 1)) |> List.map toFloat)

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
                    (round x)
                    (round ((y - modelCfg.blocksDistFromTopY) / modelCfg.blockDistY))
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
    , fallingBlocks : List FallingBlock
    , columnStates : List ColumnState
    , windowDimensions : Browser.Dom.Viewport
    }

defaultModel : Model
defaultModel =
    { state = Play
    , blocks = List.map (((*) modelCfg.blockDistY))
            ((List.range 0 (modelCfg.blockRows - 1)) |> List.map toFloat) 
            |> List.map (add modelCfg.blocksDistFromTopY)
            |> List.map (blockRow 640)
            |> List.concat
    , fallingBlocks = fallingBlockRow 640
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
    | ColorInF FallingBlock
    | SpawnMore FallingBlock
    | OnFallingBlockClick FallingBlock


--generatorWordIndexBlock : Block -> Random.Generator (Int, Block)
--generatorWordIndexBlock b =
--    (Random.int 0 (List.length allWordList - 1), b)

randomWordListIndex : Block -> Random.Generator (Int, Block)
randomWordListIndex b =
    Random.pair (Random.int 0 (List.length allWordList - 1)) (Random.constant b)

run : Msg -> Cmd Msg
run m =
    Task.perform (always m) (Task.succeed ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

        ViewportSize newSize ->
            ( { model | windowDimensions = newSize }, Cmd.none)

        OnAnimationFrame dt ->
            ( { model | fallingBlocks =
                List.map
                    ( \x ->
                        { x | y = x.y - x.vy }
                    )
                    model.fallingBlocks
                }
                
                , 
                
                Cmd.batch 
                (List.map 
                    (\fb -> 
                        if (fb.y > 250) then
                            run (ColorInF fb)
                        else
                            Cmd.none
                    )
                model.fallingBlocks)
            )

            {-
            |> update 
                    (List.map 
                        (\fb -> 
                            if (fb.y < 250) then
                                ColorInF fb
                            else
                                NoOp
                        )
                    model.fallingBlocks)
            -}
            --250

              {-Cmd.batch 
                (List.map 
                    (\fb -> 
                        if (fb.y < 250) then
                            ColorInF fb
                        else
                            Cmd.none
                    )
                model.fallingBlocks)
            ) -}
        
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
        
        OnFallingBlockClick fb ->
            ( model
                |> removeFallingBlock fb
                |> spawnNewFallingBlockRow
            , Cmd.none    
            )

        ColorInF fb ->
            ( { model | fallingBlocks =
                List.filter 
                    ( \x -> not (
                        x.x == fb.x && 
                        x.y > fb.y + modelCfg.fallingBlockSpeed &&
                        x.y < fb.y - modelCfg.fallingBlockSpeed * 2
                        )
                    )
                    model.fallingBlocks
                }
            , Cmd.none
            )

            {-
            ( { model | fallingBlocks =
                List.map 
                    ( \x -> 
                        if x.x == fb.x then
                            { x | color = "green" }
                        else
                            x
                    )
                    model.fallingBlocks
                }
            , Cmd.none
            )
            
            -}

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
        
        SpawnMore fb ->
            ( { model | fallingBlocks =
                List.append (fallingBlockRow 640) model.fallingBlocks
                }
            , Cmd.none
            )
        


removeFallingBlock : FallingBlock -> Model -> Model
removeFallingBlock fb model =
    let
        isSameBlock x =
            x.x == fb.x && 
            x.y > fb.y + modelCfg.fallingBlockSpeed &&
            x.y < fb.y - modelCfg.fallingBlockSpeed * 2
            
    in
        { model | fallingBlocks =
                    List.filter 
                        ( \x -> not (isSameBlock x)
                        )
                        model.fallingBlocks
        }

spawnNewFallingBlockRow : Model -> Model
spawnNewFallingBlockRow model =
    { model | fallingBlocks =
                List.append (fallingBlockRow 640) model.fallingBlocks
    }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [
            displayAll model
        ]
    
displayAll : Model -> Html Msg
displayAll model =
    displayFullScreen model.windowDimensions 
                (List.append 
                    (displayFallingBlocks model)
                    (displayBlocks model)
                    
                )


displayFullScreen : Browser.Dom.Viewport -> List ( Html Msg ) -> Html Msg
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

displayFallingBlocks : Model -> List (Html Msg)
displayFallingBlocks ({fallingBlocks} as model) = 
    let
        fallingBlockRects =
            List.map (\fb ->
                        Svg.g
                            []
                            [ Svg.rect
                                [ SvgA.width (String.fromFloat fb.w)
                                , SvgA.height (String.fromFloat fb.h)
                                , SvgA.x (String.fromFloat fb.x)
                                , SvgA.y (String.fromFloat fb.y)
                                , SvgA.rx (String.fromFloat modelCfg.blockCornerRounding)
                                , SvgA.fill fb.color
                                , SvgE.onClick (OnFallingBlockClick fb)
                                ]
                                []
                            , Svg.text_
                                [ SvgA.x (String.fromFloat (fb.x + 20))
                                , SvgA.y (String.fromFloat (fb.y + 26))
                                , SvgA.fontSize "30"
                                , SvgA.fill "white"
                                ]
                                [ Svg.text ( getWordName fb.word) ]
                            ]
                        ) fallingBlocks
    in 
        fallingBlockRects

displayBlocks : Model -> List (Html Msg)
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
                                [ Svg.text ( getWordName b.word) ]
                            
                            ]
                        ) blocks
    in
        blockRects


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

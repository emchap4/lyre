module Main exposing (main)

import Array
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Random
import Random.Array
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Task
import Time


--import Collage
--import Element\


allWordList : List Word
allWordList =
    [ Noun "you" "test1"
    , Verb "bean" "test2"
    , Noun "man" "test3"
    , Noun "never" ""
    , Noun "gonna" ""
    , Noun "give" ""
    , Noun "up" ""
    , Noun "1" ""
    , Noun "2" ""
    , Noun "3" ""
    , Noun "4" ""
    , Noun "5" ""
    , Noun "6" ""
    , Noun "7" ""
    , Noun "8" ""
    , Noun "9" ""
    , Noun "10" ""
    , Noun "11" ""
    , Noun "12" ""
    , Noun "13" ""
    , Noun "14" ""
    ]

while : (a -> Bool) -> a -> (a -> a) -> a
while condition initialState body =
    if not (condition initialState) then
        initialState
    else 
        while condition (body initialState) body

filterAllWordList : List Word -> Int -> Random.Seed -> Bool -> List Word -> List Word
filterAllWordList initialwl len seed allowRepeats blacklistwl =
    let
        lengthIsNotLen =
            \{n} -> len /= List.length n
        initialFilteredList =
            List.filter (isNotInBlacklistedWords blacklistwl) initialwl

        randomIndex : (Int, Random.Seed)
        randomIndex =
            (Random.step (Random.int 0 (List.length initialFilteredList)) seed)

        returnwl :
            { n : List Word
            , iterations : Int
            , filteredList : List Word
            , nextWordUp : Maybe Word
            , newSeed : Random.Seed
            }
        returnwl =
            { n = []
            , iterations = 0 
            , filteredList = initialFilteredList
            , nextWordUp = 
                Array.fromList initialFilteredList
                    |> Array.get (Tuple.first randomIndex)
            , newSeed = Tuple.second randomIndex
            } --recursively 

        result =
            while lengthIsNotLen returnwl 
                ( \{n, iterations, filteredList, nextWordUp, newSeed} ->
                    case nextWordUp of
                        Just word ->
                            if not allowRepeats then
                                { n = word :: n
                                , iterations = iterations + 1
                                , filteredList = List.filter 
                                                    (\w -> w /= word)
                                                    filteredList
                                , nextWordUp = Array.fromList filteredList
                                                |> Array.get 
                                                    (Tuple.first 
                                                        (Random.step (Random.int 0 (List.length filteredList - 1)) newSeed)
                                                    )
                                , newSeed = Tuple.second (Random.step (Random.int 0 (List.length filteredList - 1)) newSeed) 
                                }
                            else 
                                { n = word :: n
                                , iterations = iterations + 1
                                , filteredList = filteredList
                                , nextWordUp = Array.fromList filteredList
                                                |> Array.get 
                                                    (Tuple.first 
                                                        (Random.step (Random.int 0 (List.length filteredList - 1)) newSeed)
                                                    )
                                , newSeed = Tuple.second (Random.step (Random.int 0 (List.length filteredList - 1)) newSeed) 
                                }
                        Nothing ->
                            { n = (Noun "boo2" "") :: n
                            , iterations = iterations + 1
                            , filteredList = filteredList
                            , nextWordUp = Array.fromList filteredList
                                                |> Array.get 
                                                    (Tuple.first 
                                                        (Random.step (Random.int 0 (List.length filteredList)) newSeed)
                                                    )
                                , newSeed = Tuple.second (Random.step (Random.int 0 (List.length filteredList)) newSeed) 
                            }
                )

    in
        result.n




{--
filterAllWordList : List Word -> Int -> Int -> Bool -> List Word -> List Word
filterAllWordList initialwl len rn allowRepeats blacklistwl =
    let
        lengthIsNotLen =
            \{n} -> len /= List.length n
        initialFilteredList =
            List.filter (isNotInBlacklistedWords blacklistwl) initialwl
        returnwl :
            { n : List Word
            , iterations : Int
            , filteredList : List Word
            , nextWordUp : Maybe Word
            }
        returnwl =
            { n = []
            , iterations = 0 
            , filteredList = initialFilteredList
            , nextWordUp = 
                Array.fromList initialFilteredList
                    |> Array.get (rn - 0)
            } --recursively 

        -- nextWordUp : Maybe Word
        -- nextWordUp =
        --     Array.fromList returnwl.filteredList
        --         |> Array.get (rn - returnwl.iterations)

        result =
            while lengthIsNotLen returnwl 
                ( \{n, iterations, filteredList, nextWordUp} ->
                    case nextWordUp of
                        Just word ->
                            if not allowRepeats then
                                { n = word :: n
                                , iterations = iterations + 1
                                , filteredList = List.filter 
                                                    (\w -> w /= word)
                                                    filteredList
                                , nextWordUp = Array.fromList filteredList
                                                |> Array.get (rn - iterations)
                                }
                            else 
                                { n = word :: n
                                , iterations = iterations + 1
                                , filteredList = filteredList
                                , nextWordUp = Array.fromList filteredList
                                                |> Array.get (rn - iterations)
                                }
                        Nothing ->
                            { n = (Noun "boo" "") :: n
                            , iterations = iterations + 1
                            , filteredList = filteredList
                            , nextWordUp = Array.fromList filteredList
                                                |> Array.get (rn - iterations)
                            }
                )

    in
        result.n
--}

flipFlopAddSubtract : Bool -> (Int -> Int -> Int)
flipFlopAddSubtract toAdd =
    if toAdd then
        (+)
    else
        (-)


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel
    , Cmd.batch
        [ getViewportCommand
        -- , Cmd.batch
        --     (List.map (\b -> Random.generate RandomWord (randomWordListIndex b))
        --         (List.map ((*) modelCfg.blockDistY)
        --             (List.range 0 (modelCfg.blockRows - 1) |> List.map toFloat)
        --             |> List.map (add modelCfg.blocksDistFromTopY)
        --             |> List.map (blockRow 640)
        --             |> List.concat
        --         )
        --     )
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
    , fallingBlockSpawnHeight = -100 -- -500
    , fallingBlockSpeed = -1
    , fallingBlockCutoff = 375
    }


type alias Positioned a =
    { a
        | x : Float
        , y : Float
    }

type alias GridPositioned a =
    { a
        | gridX : Int
        , gridY : Int
    }

type alias Moving a =
    { a | vy : Float }

type alias Sized a =
    { a
        | w : Float
        , h : Float
    }

type alias Colored a =
    { a | color : String }

type alias Labeled a =
    { a | word : Maybe Word }

type alias WithAnswers a =
    { a | answers : List Word }

type alias WithRandomWords a =
    { a | randomWords : List Word}

type alias WithClosestFallingBlock a =
    { a | closestFallingBlock : Maybe FallingBlock }

type alias OccupiesColumns a =
    { a | occupiedColumns : Int }

type alias Block =
    WithClosestFallingBlock ( Labeled ( Colored ( Sized ( GridPositioned ( Positioned {} ) ) ) ) )  

type alias FallingBlock =
    Moving ( WithAnswers ( WithRandomWords ( Colored ( OccupiesColumns ( Sized ( GridPositioned ( Positioned {} ) ) ) ) ) ) )

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


block : Float -> Float -> Int -> Int -> Float -> Float -> Maybe Word -> String -> Maybe FallingBlock -> Block
block x y gx gy w h word color closestFallingBlock =
    { x = x, y = y, gridX = gx, gridY = gy, w = w, h = h, word = word, color = color, closestFallingBlock = closestFallingBlock }


fallingBlock : Float -> Float -> Float -> Int -> Int -> Float -> Float -> Int -> String -> List Word -> List Word -> FallingBlock
fallingBlock x y vy gx gy w h occupiedColumns color answers randomWords =
    { x = x, y = y, gridX = gx, gridY = gy, w = w, h = h, occupiedColumns = occupiedColumns, color = color, vy = vy, answers = answers, randomWords = randomWords}

--getRandomWordList : Float -> 
--getRandomWordList rwAmount blacklist

--filterAllWordList initialwl len rn allowRepeats blacklistwl
--filterAllWordList initialwl len seed allowRepeats blacklistwl

fallingBlockRow : Float -> List FallingBlock
fallingBlockRow viewportX =
    let
        xOff =
            toFloat (-modelCfg.blockCols // 2 |> toFloat |> ceiling)
                * modelCfg.blockDistX
                + viewportX
                - modelCfg.blockWidth
                / 2

        occupiedColumns =
            (modelCfg.blockWidth + (modelCfg.blockDistX - modelCfg.blockWidth)) / modelCfg.blockDistX
            --change first blockWidth to local fallingBlock width
    in
    List.map
        (\x ->
            fallingBlock 
                (modelCfg.blockDistX * x + xOff)
                modelCfg.fallingBlockSpawnHeight
                modelCfg.fallingBlockSpeed
                (round x)
                (round modelCfg.fallingBlockSpawnHeight)
                modelCfg.blockWidth
                modelCfg.blockHeight
                (round occupiedColumns)
                "purple"
                [Verb "bean" "test2"] -- Change to be answers
                (filterAllWordList allWordList (round (modelCfg.blockRows * occupiedColumns)) (Random.initialSeed (round x)) False [Verb "bean" "test2"])
        ) -- FOR SOME REASON, (ROUND X) RETURNS (MODELCFG.BLOCKCOLS - 1)... it shouldn't, but it does
        (List.range 0 (modelCfg.blockCols - 1) |> List.map toFloat)


blockRow : Float -> Float -> List Block
blockRow viewportX y =
    let
        xOff =
            toFloat (-modelCfg.blockCols // 2 |> toFloat |> ceiling)
                * modelCfg.blockDistX
                + viewportX
                - modelCfg.blockWidth
                / 2
    in
    List.map
        (\x ->
            block (modelCfg.blockDistX * x + xOff)
                y
                (round x)
                (round ((y - modelCfg.blocksDistFromTopY) / modelCfg.blockDistY))
                modelCfg.blockWidth
                modelCfg.blockHeight
                (Just (Noun "" ""))
                "purple"
                Nothing
        )
        (List.range 0 (modelCfg.blockCols - 1) |> List.map toFloat)


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
    , blocks =
        List.map ((*) modelCfg.blockDistY)
            (List.range 0 (modelCfg.blockRows - 1) |> List.map toFloat)
            |> List.map (add modelCfg.blocksDistFromTopY)
            |> List.map (blockRow 640)
            |> List.concat
    , fallingBlocks = fallingBlockRow 640
    , columnStates = List.repeat modelCfg.blockCols AllActive
    , windowDimensions =
        { scene =
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
--    | FindRandomWord Block
--    | RandomWord ( Int, Block )
    | ColorIn Block
    | ColorInF FallingBlock
    | SpawnMore FallingBlock
    | OnFallingBlockClick FallingBlock



--generatorWordIndexBlock : Block -> Random.Generator (Int, Block)
--generatorWordIndexBlock b =
--    (Random.int 0 (List.length allWordList - 1), b)


randomWordListIndex : Block -> Random.Generator ( Int, Block )
randomWordListIndex b =
    Random.pair (Random.int 0 (List.length allWordList - 1)) (Random.constant b)

run : Msg -> Cmd Msg
run m =
    Task.perform (always m) (Task.succeed ())



findClosestFallingBlock : Model -> Block -> Maybe FallingBlock
findClosestFallingBlock model b =
    let
        fallingBlocksInColumn : List FallingBlock
        fallingBlocksInColumn =
            List.filter 
                ( \fb -> fb.x >= b.x )--fb.x >= b.x && (fb.x + fb.w) <= b.x )
                model.fallingBlocks
    in
        List.sortBy .y fallingBlocksInColumn |> List.reverse |> List.head

{-
Array.fromList (filterAllWordList allWordList 21 rn False [])
                        |> Array.get rn
                        -}


onAnimationFrameModel : Model -> Model
onAnimationFrameModel model =
    { model
        | fallingBlocks =
            List.map
                (\x -> { x | y = x.y - x.vy })
                model.fallingBlocks
                |> List.filter (\fb -> fb.y < modelCfg.fallingBlockCutoff)
        , blocks =
            List.map
                (\b -> 
                    case (findClosestFallingBlock model b) of 
                        Just fb ->
                            { b | word = 
                                Array.fromList (fb.randomWords) 
                                    |> Array.get (b.gridY + (modelCfg.blockRows * (fb.occupiedColumns - 1)))
                            }
                        Nothing ->
                            { b | word = Just (Noun "boo" "") }
                )
                model.blocks

        -- Removes the block if it falls below a certain point
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ViewportSize newSize ->
            ( { model | windowDimensions = newSize }, Cmd.none )

        OnAnimationFrame _ ->
            ( onAnimationFrameModel model
            , Cmd.none
            )

        ColorIn b ->
            ( { model
                | blocks =
                    List.map
                        (\x ->
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
            ( { model
                | fallingBlocks =
                    List.filter
                        (\x ->
                            not
                                (x.x
                                    == fb.x
                                    && x.y
                                    > fb.y
                                    + modelCfg.fallingBlockSpeed
                                    && x.y
                                    < fb.y
                                    - modelCfg.fallingBlockSpeed
                                    * 2
                                )
                        )
                        model.fallingBlocks
              }
            , Cmd.none
            )

        --FindRandomWord b ->
        --    ( model
        --    , Random.generate RandomWord (randomWordListIndex b)
        --    )

        {-RandomWord ( rn, b ) ->
            let
                selectedWord = --turn to list/array and get the word based on id/increment
                    Array.fromList (filterAllWordList allWordList 21 rn False [])
                        |> Array.get rn --List.map it to a List.range so that rn increments 
            in
            ( { model
                | blocks =
                    List.map
                        (\x ->
                            if x.x == b.x && x.y == b.y then
                                case selectedWord of
                                    Just word ->
                                        { x | word = word }

                                    --Noun "test3" "test" }
                                    Nothing ->
                                        x

                            else
                                x
                        )
                        model.blocks
              }
            , Cmd.none
            )-}

        SpawnMore _ ->
            ( { model
                | fallingBlocks =
                    List.append (fallingBlockRow 640) model.fallingBlocks
              }
            , Cmd.none
            )


removeFallingBlock : FallingBlock -> Model -> Model
removeFallingBlock fb model =
    let
        isSameBlock x =
            x.x
                == fb.x
                && x.y
                > fb.y
                + modelCfg.fallingBlockSpeed
                && x.y
                < fb.y
                - modelCfg.fallingBlockSpeed
                * 2
    in
    { model
        | fallingBlocks =
            List.filter
                (\x -> not (isSameBlock x))
                model.fallingBlocks
    }


spawnNewFallingBlockRow : Model -> Model
spawnNewFallingBlockRow model =
    { model
        | fallingBlocks =
            List.append (fallingBlockRow 640) model.fallingBlocks
    }



-- VIEW


view : Model -> Html Msg
view model =
    displayFullScreen model.windowDimensions
        (List.append
            (displayFallingBlocks model)
            (displayBlocks model)
        )


displayAll : Model -> Html Msg
displayAll model =
    displayFullScreen model.windowDimensions
        (List.append
            (displayFallingBlocks model)
            (displayBlocks model)
        )


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
        [ SvgA.viewBox "0 0 1275 570" --("0 0" ++ String.fromFloat width ++ " " ++ String.fromFloat height_)--"0 0 640 480"--("0 0 " ++ String.fromInt modelCfg.gameWidth ++ " " ++ String.fromInt modelCfg.gameHeight)
        , SvgA.preserveAspectRatio "xMidYMax"
        , SvgA.width (String.fromFloat width)
        , SvgA.height (String.fromFloat height_)
        ]
        content


displayFallingBlocks : Model -> List (Html Msg)
displayFallingBlocks { fallingBlocks } =
    let
        fallingBlockRects =
            List.map
                (\fb ->
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
                            [] --Svg.text (getWordName fb.word) ]
                        ]
                )
                fallingBlocks
    in
    fallingBlockRects


displayBlocks : Model -> List (Html Msg)
displayBlocks { blocks } =
    let
        blockRects =
            List.map
                (\b ->
                    Svg.g
                        []
                        [ Svg.rect
                            [ SvgA.width (String.fromFloat b.w)
                            , SvgA.height (String.fromFloat b.h)
                            , SvgA.x (String.fromFloat b.x)
                            , SvgA.y (String.fromFloat b.y)
                            , SvgA.rx (String.fromFloat modelCfg.blockCornerRounding)
                            , SvgA.fill b.color
                            --, SvgE.onClick (FindRandomWord b) --SvgE.onClick (ColorIn b)
                            ]
                            []
                        , Svg.text_
                            [ SvgA.x (String.fromFloat (b.x + 20))
                            , SvgA.y (String.fromFloat (b.y + 26))
                            , SvgA.fontSize "30"
                            , SvgA.fill "white"
                            ]
                            [ 
                                Svg.text 
                                    (
                                        case b.word of
                                            Just word ->
                                                getWordName word
                                            Nothing ->
                                                ""
                                    )
                            ]
                        ]
                )
                blocks
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
        , Browser.Events.onResize
            (\w h ->
                ViewportSize
                    { scene = { width = toFloat w, height = toFloat h}
                    , viewport = { x = 0, y = 0, width = toFloat w, height = toFloat h }
                    }
            )
        ]

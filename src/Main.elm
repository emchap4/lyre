module Main exposing (main)

import Html exposing (..)
import Html.Attributes as HtmlA
import Browser
import Browser.Dom
import Browser.Events
import Platform exposing (Task)
import Task
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
--import Collage
--import Element
import Browser.Dom exposing (Viewport)

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
    , blockWidth = 50
    , blockHeight = 20
    , blockDistX = 80
    , blockDistY = 33
    , blockCols = 5
    , blockRows = 3
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

blockRow : Float -> List Block
blockRow y =
    let
        xOff =
            toFloat (-modelCfg.blockCols // 2 |> toFloat |> ceiling)
                * modelCfg.blockDistX
    in
        List.map
            (\x ->
                block (modelCfg.blockDistX * x )--+ xOff)
                    y
                    0 0
                    modelCfg.blockWidth
                    modelCfg.blockHeight
                    (Noun "test" "test2")
                    "purple"
            ) ((List.range 0 (modelCfg.blockCols - 1)) |> List.map toFloat)



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
    , blocks = List.map ((*) modelCfg.blockDistY)
            ((List.range 0 (modelCfg.blockRows - 1)) |> List.map toFloat)
            |> List.map blockRow
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
    | ColorIn Block


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

-- VIEW

view : Model -> Html Msg
view model =
    displayBlocks model


displayFullScreen : Browser.Dom.Viewport -> List (Html Msg) -> Html Msg
displayFullScreen v content =
    let
        -- to prevent scrolling down when user hits space bar
        height_ =
            v.viewport.height - 20

        width =
            v.viewport.width

        gameScale =
            Basics.min (width / modelCfg.gameWidth)
                (height_ / modelCfg.gameHeight)
    in
        Svg.svg
            [ SvgA.width (String.fromFloat width)
            , SvgA.height (String.fromFloat height_)
            , SvgA.scale (String.fromFloat gameScale)
            ]
            content

displayBlocks : Model -> Html Msg
displayBlocks ({ blocks } as model) = 
    let 
        blockRects =
            List.map (renderBlocks model) blocks
    in
        displayFullScreen model.windowDimensions blockRects

renderBlocks : Model -> Block -> Html Msg
renderBlocks model b =
    Svg.rect   
        [ SvgA.width (String.fromFloat b.w)
        , SvgA.height (String.fromFloat b.h)
        , SvgA.x (String.fromFloat b.x)
        , SvgA.y (String.fromFloat b.y)
        , SvgA.fill b.color
        , SvgE.onClick (ColorIn b)
        ]
        []





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



{--

view : Model -> Html Msg
view model =
    displayBlocks model |> Element.layout []


displayFullScreen : Browser.Dom.Viewport -> Collage.Collage Msg -> Element.Element Msg
displayFullScreen { width, height } content =
    let
        -- to prevent scrolling down when user hits space bar
        height_ =
            height - 20

        gameScale =
            Basics.min (toFloat width / modelCfg.gameWidth)
                (toFloat height_ / modelCfg.gameHeight)
    in
        collage width height_ [ content |> Collage.scale gameScale ]

displayBlocks : Model -> Element.Element Msg
displayBlocks ({ blocks } as model) = 
    let 
        blockRects =
            Collage.group 
                <| List.map (\b -> Collage.rectangle b.w b.h |> make b) blocks
    in
        displayFullScreen model.windowDimensions blockRects

make : Positioned a -> Collage.Shape -> Collage.Collage Msg
make obj shape = 
    shape |> Collage.shift (obj.x, obj.y)













List.map (\b -> Svg.rect
                                    [ SvgA.width (String.fromFloat b.w)
                                    , SvgA.height (String.fromFloat b.h)
                                    , SvgA.x (String.fromFloat b.x)
                                    , SvgA.y (String.fromFloat b.y)
                                    , 
                                    , SvgE.onClick ColorIn
                                    ]
                                    []) blocks



List.map (\b -> Html.div
                                    [ HtmlA.width (round b.w)
                                    , HtmlA.height (round b.h)
                                    , HtmlA.style "left" (String.fromFloat b.x)
                                    , HtmlA.style "left" (String.fromFloat b.y)
                                    , HtmlA.style "fill" (b.color)
                                    , SvgE.onClick ColorIn
                                    ]
                                    []) blocks
--}
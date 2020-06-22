module Main exposing (main)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
-- import Html.Styled.Events exposing (onClick)
import Css exposing (..)
import Css.Transitions
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Random
import Random.Set
import Array
import Set
import Html.Attributes exposing (value)

import Debug

boardSize = 4
gameBoardWidth = 500
cellSize = 107
cellMarginSize = 15
occupiedCell = -1000

flat : List ( List a ) -> List a
flat list =
    List.foldr (++) [] list
type Direction
    = Left
    | Right
    | Up
    | Down
    | Other

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Msg
toDirection string =
  case string of
    "ArrowLeft" ->
        KeyDown Left

    "ArrowRight" ->
        KeyDown Right

    "ArrowUp" ->
        KeyDown Up

    "ArrowDown" ->
        KeyDown Down

    _ ->
        KeyDown Other

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Cell =
    { x : Int
    , y : Int
    , value: Int
    }

type alias GameGrid = List ( List ( Maybe Cell ) )

type alias Model =
    { bestScore : Int
    , size : Int
    , grid : GameGrid
    , score : Int
    , won : Bool
    }

emptyGrid : Int -> GameGrid
emptyGrid size =
    List.repeat size <| List.repeat size Nothing

randomPoint : Int -> GameGrid -> Int -> Random.Generator ( List Cell )
randomPoint count grid size =
    let
        cells = flat grid

        _ = Debug.log "CELLS: " <| String.join
            ", "
            (
                List.map
                    (\v ->
                        case v of
                            Just value ->
                                String.fromInt value.value

                            Nothing ->
                                "0"
                    )
                    cells
            )

        nonUsedCellsIndexes =
            List.indexedMap
                (\index cell ->
                    case cell of
                        Just _ ->
                            occupiedCell

                        Nothing ->
                            index
                )
                cells

        _ = Debug.log
            "NON_USED_CELLS_INDEXES"
            <| String.join ", " <| List.map String.fromInt nonUsedCellsIndexes

        indexes = List.filter (\index -> index /= occupiedCell) nonUsedCellsIndexes

        _ = Debug.log
            "NON_OCCUPIED_CELLS"
            <| String.join ", " <| List.map String.fromInt indexes

        indexToNonUsedCellIndex : Int -> Int
        indexToNonUsedCellIndex index =
            case Array.get index <| Array.fromList indexes of
                Just value ->
                    value

                Nothing ->
                    0

        mapIndexesToCells : Set.Set Int -> List Float -> List Cell
        mapIndexesToCells set values =
            let
                _ = Debug.log
                    "RANDOM VALUES"
                    <| String.join ", " <| List.map String.fromInt <| Set.toList set
            in
                List.indexedMap
                    (\index value ->
                        let
                            nonUsedCellIndex = indexToNonUsedCellIndex value

                            _ = Debug.log "INDEX: " value
                            _ = Debug.log "NON_USED_CELL_INDEX" nonUsedCellIndex

                            cellValue =
                                case Array.get index <| Array.fromList values of
                                    Just v ->
                                        if v < 0.9 then 2 else 4

                                    Nothing ->
                                        0

                            y = nonUsedCellIndex // size

                            x = nonUsedCellIndex - y * size
                        in
                            { x = x, y = y, value = cellValue }
                    )
                    ( Set.toList set )
    in
        Random.map2
            mapIndexesToCells
            ( Random.Set.set count ( Random.int 0 <| ( List.length indexes ) - 1 ) )
            ( Random.list count ( Random.float 0 1 ) )

init : () -> ( Model, Cmd Msg )
init () =
    let
        initialGrid = emptyGrid boardSize
    in
        (
            { grid = initialGrid
            , score = 0
            , won = False
            , bestScore = 0
            , size = boardSize -- can be configurable one day
            }
            , Random.generate RandomCells ( randomPoint 2 initialGrid boardSize )
        )

-- UPDATE

type Msg =
    KeyDown Direction
    | RandomCell Cell
    | Reset
    | InitialState ( List ( Cell ) )
    | RandomCells ( List ( Cell ) )

replaceCell : List ( Cell ) -> Int -> Int -> Maybe Cell -> Maybe Cell
replaceCell cells rowIndex columnIndex oldCell =
    let
        newCell = List.head <| List.filter (\c -> c.x == columnIndex && c.y == rowIndex) cells
    in
        case newCell of
            Just _ ->
                newCell

            Nothing ->
                oldCell

setCells : List ( Cell ) -> ( GameGrid -> GameGrid )
setCells cells =
    List.indexedMap
        (\rowIndex row ->
            List.indexedMap (\columnIndex cell -> replaceCell cells rowIndex columnIndex cell ) row
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown direction ->
            case direction of
                Up ->
                    ( model, Random.generate RandomCells ( randomPoint 1 model.grid model.size ) )

                Right ->
                    ( model, Random.generate RandomCells ( randomPoint 1 model.grid model.size ) )

                Down ->
                    ( model, Random.generate RandomCells ( randomPoint 1 model.grid model.size ) )

                Left ->
                    ( model, Random.generate RandomCells ( randomPoint 1 model.grid model.size ) )

                Other ->
                    ( model, Cmd.none )
        Reset ->
            ( model, Cmd.none )

        RandomCell cell ->
            ( { model | grid = setCells [ cell ] model.grid }, Cmd.none )

        RandomCells cells ->
            ( { model | grid = setCells cells model.grid }, Cmd.none )

        InitialState cells ->
            ( { model | grid = setCells cells model.grid }, Cmd.none )

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder

--VIEW

view : Model -> Html Msg
view model =
    div
        []
        [ viewHeader model.score model.bestScore
        , viewGameContainer model
        , viewFooter model.grid
        ]

viewHeader : Int -> Int -> Html Msg
viewHeader score bestScore =
    div []
        [ text "2048"
        , text <| String.fromInt score
        , text <| String.fromInt bestScore
        ]

viewGameContainer : Model -> Html Msg
viewGameContainer model =
    div
        [ css
            [ position relative
            , padding <| px cellMarginSize
            , margin auto
            , marginTop <| px 40
            , backgroundColor <| hex "#BBADA0"
            , borderRadius <| px 6
            , width <| px gameBoardWidth
            , height <| px gameBoardWidth
            , boxSizing borderBox
            , color <| hex "776e65"
            , fontFamilies [ "Clear Sans", "Helvetica Neue", "Arial", "sans-serif" ]
            ]
        ]
        [ viewGrid model.size
        , viewTiles model.size model.grid
        ]

viewGrid : Int -> Html Msg
viewGrid size =
    div
        [ css
            [ position absolute
            , zIndex <| int 1
            ]
        ]
        <|
            List.repeat size ( viewGridRow size )

viewGridRow : Int -> Html Msg
viewGridRow size =
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , marginBottom <| px cellMarginSize
            ]
        ]
        <|
            List.repeat size ( viewGridCell () )

viewGridCell : () -> Html Msg
viewGridCell _ =
    div
        [ css
            [ marginRight <| px cellMarginSize
            , width <| px 106.25
            , height <| px 106.25
            , borderRadius <| px 3
            , backgroundColor <| rgba 238 228 218 0.35
            ]
        ] []

viewTiles : Int -> GameGrid -> Html Msg
viewTiles size grid =
    div
        [ css
            [ position absolute
            , zIndex <| int 2
            ]
        ]
        (
            grid
                |> flat
                |> List.filter
                    (\value ->
                        case value of
                            Just _ ->
                                True

                            Nothing ->
                                False
                    )
                |> List.map viewTile
        )

viewTile : Maybe Cell -> Html Msg
viewTile cell =
    case cell of
        Just cellValue ->
            let
                wholeCellSize = cellSize + cellMarginSize
                x = cellValue.x
                y = cellValue.y
                value = cellValue.value
            in
                div
                    [ css
                        [ position absolute
                        , width <| px cellSize
                        , height <| px cellSize
                        , lineHeight <| px cellSize
                        , transform <| translate2 ( px ( toFloat <|  x * wholeCellSize ) ) ( px ( toFloat <| y * wholeCellSize ) )
                        , Css.Transitions.transition
                            [ Css.Transitions.transform3 100 0 Css.Transitions.easeInOut ]
                        ]
                    ]
                    [ div
                        [ css
                            [ borderRadius <| px 3
                            , backgroundColor <| hex "EEE4DA"
                            , textAlign center
                            , fontWeight bold
                            , fontSize <| px 55
                            , zIndex <| int 10
                            ]
                        ]
                        [ text <| String.fromInt value ]
                    ]

        Nothing ->
            text ""

viewFooter : GameGrid -> Html Msg
viewFooter grid =
    let
        cells = List.map
            (\value ->
                case value of
                    Just cell ->
                        String.fromInt cell.value

                    Nothing ->
                        "0"
            )
            <| flat grid
    in
        div
            []
            [ text <| String.join ", " cells ]

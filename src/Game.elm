module Game exposing
    ( Direction(..)
    , Model
    , Msg
    , init
    , move
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (onKeyDown)
import Css exposing (..)
import Css.Media
import Css.Transitions
import Html.Attributes exposing (value)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Json.Decode as D exposing (Decoder)
import Random
import RandomCell exposing (Cell, randomCells)
import Svg.Styled.Attributes exposing (k)



-- TYPES


type alias Row =
    List (Maybe Int)


type alias GameGrid =
    List Row


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


type Model
    = Internals
        { bestScore : Int
        , size : Int
        , grid : GameGrid
        , score : Int
        , won : Bool
        , failed : Bool
        , targetScore : Int
        }



-- MODEL


gridSize : Int
gridSize =
    4


initGrid : Int -> GameGrid
initGrid size =
    List.repeat size Nothing
        |> List.repeat size


init : () -> ( Model, Cmd Msg )
init () =
    let
        grid =
            initGrid gridSize
    in
    ( Internals
        { bestScore = 0
        , size = gridSize
        , grid = grid
        , score = 0
        , won = False
        , failed = False
        , targetScore = 2048
        }
    , Random.generate GotNewCells (randomCells 2 (availableCells grid))
    )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder



-- UPDATE


type Msg
    = KeyDown Direction
    | Reset
    | GotNewCells (List Cell)
    | Won
    | Failed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Internals model) =
    case msg of
        KeyDown Other ->
            ( Internals model, Cmd.none )

        KeyDown direction ->
            let
                _ =
                    Debug.log "direction" direction

                wonOrFailed =
                    model.won || model.failed

                _ =
                    Debug.log "won or failed" wonOrFailed

                newGrid =
                    if wonOrFailed then
                        model.grid

                    else
                        move direction model.grid

                _ =
                    Debug.log "new updated grid" newGrid

                isNothingChanged =
                    wonOrFailed
                        || (model.grid
                                |> List.concat
                                |> (==) (List.concat newGrid)
                           )

                -- newGrid =
                -- if won then nothing
                -- if failed then nothing
                -- if nothing changes then nothing
                --
                newModel =
                    if isNothingChanged then
                        Internals model

                    else
                        Internals { model | grid = newGrid }

                cmd =
                    if isNothingChanged then
                        Cmd.none

                    else
                        Random.generate GotNewCells (randomCells 1 (availableCells newGrid))
            in
            ( newModel, cmd )

        Reset ->
            init ()

        GotNewCells cells ->
            let
                grid =
                    withCells cells model.grid

                won =
                    isWon model.targetScore grid

                failed =
                    not won && isFailed model.targetScore grid
            in
            ( Internals
                { model
                    | grid = grid
                    , won = won
                    , failed = failed
                }
            , Cmd.none
            )

        Won ->
            ( Internals model, Cmd.none )

        Failed ->
            ( Internals model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view (Internals model) =
    div
        []
        [ viewHeader model.score model.bestScore
        , viewGameContainer <| Internals model
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
viewGameContainer (Internals model) =
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
        List.repeat size (viewGridRow size)


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
        List.repeat size (viewGridCell ())


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
        ]
        []


viewTiles : Int -> GameGrid -> Html Msg
viewTiles size grid =
    let
        withIndexes rowI colI value =
            { x = colI, y = rowI, value = value }

        nonEmptyCells =
            grid
                |> List.indexedMap (\rowI -> List.indexedMap <| withIndexes rowI)
                |> List.concat
                |> List.filter (\{ value } -> value /= Nothing)
    in
    div
        [ css
            [ position absolute
            , zIndex <| int 2
            ]
        ]
        (List.map viewTile nonEmptyCells)


viewTile : { x : Int, y : Int, value : Maybe Int } -> Html Msg
viewTile cell =
    let
        wholeCellSize =
            cellSize + cellMarginSize
    in
    div
        [ css
            [ position absolute
            , width <| px cellSize
            , height <| px cellSize
            , lineHeight <| px cellSize
            , transform <| translate2 (px (toFloat cell.x * wholeCellSize)) (px (toFloat cell.y * wholeCellSize))
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
            [ (text << Maybe.withDefault "" << Maybe.map String.fromInt) cell.value ]
        ]


viewFooter : GameGrid -> Html Msg
viewFooter grid =
    let
        cells =
            grid
                |> List.concat
                |> List.map
                    (Maybe.withDefault "0" << Maybe.map String.fromInt)
    in
    div
        []
        [ text <| String.join ", " cells ]



-- SERIALIZATION


keyDecoder : Decoder Msg
keyDecoder =
    D.map toDirection (D.field "key" D.string)


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



-- GETTERS


availableCells : GameGrid -> List ( Int, Int )
availableCells grid =
    let
        withIndexes =
            List.indexedMap Tuple.pair
    in
    grid
        |> withIndexes
        |> List.map (Tuple.mapSecond withIndexes)
        |> List.concatMap (\( rowIndex, row ) -> List.map (\( colIndex, v ) -> ( rowIndex, colIndex, v )) row)
        |> List.filter (\( _, _, v ) -> v == Nothing)
        |> List.map (\( rowIndex, colIndex, _ ) -> ( rowIndex, colIndex ))


isWon : Int -> GameGrid -> Bool
isWon targetScore grid =
    grid
        |> List.concat
        |> List.any (Maybe.withDefault False << Maybe.map ((==) targetScore))


isFailed : Int -> GameGrid -> Bool
isFailed targetScore grid =
    let
        checkRowForPossibleMove row =
            row
                |> List.foldr
                    (\cell { res, prev } ->
                        if res == True then
                            { res = res, prev = prev }

                        else
                            { res = cell == prev, prev = cell }
                    )
                    { prev = Nothing, res = False }
                |> .res

        isFull =
            grid
                |> List.concat
                |> List.filter ((==) Nothing)
                |> List.length
                |> (==) 0

        isMovePossible =
            List.foldr (\row accum -> accum || checkRowForPossibleMove row) False
    in
    not (isWon targetScore grid)
        && isFull
        && (not <| isMovePossible grid)
        && (not << isMovePossible << transpose) grid



-- TRANSFORMERS


withCells : List Cell -> GameGrid -> GameGrid
withCells cells =
    List.indexedMap
        (\rowIndex ->
            List.indexedMap
                (\colIndex val ->
                    case RandomCell.valueAt ( rowIndex, colIndex ) cells of
                        Just newValue ->
                            Just newValue

                        Nothing ->
                            val
                )
        )


transpose : GameGrid -> GameGrid
transpose grid =
    case List.head grid of
        Nothing ->
            []

        Just xs ->
            List.indexedMap
                (\index _ ->
                    List.map
                        (Maybe.withDefault Nothing
                            << List.head
                            << List.filter ((/=) Nothing)
                            << List.indexedMap
                                (\i cell ->
                                    if i == index then
                                        cell

                                    else
                                        Nothing
                                )
                        )
                        grid
                )
                xs


rotateClockwise : GameGrid -> GameGrid
rotateClockwise =
    List.map List.reverse << transpose


rotateAntiClockwise : GameGrid -> GameGrid
rotateAntiClockwise =
    transpose << List.map List.reverse


move : Direction -> GameGrid -> GameGrid
move direction =
    case direction of
        Right ->
            moveRight

        Left ->
            rotateClockwise
                << rotateClockwise
                << moveRight
                << rotateClockwise
                << rotateClockwise

        Up ->
            rotateAntiClockwise << moveRight << rotateClockwise

        Down ->
            rotateClockwise << moveRight << rotateAntiClockwise

        Other ->
            identity



-- TODO add tests


moveRight : GameGrid -> GameGrid
moveRight grid =
    let
        addEmptyCells =
            addEmpties (List.length grid)
    in
    List.map
        (addEmptyCells
            << removeEmpties
            << List.reverse
            << List.foldr
                (\cell accum ->
                    if cell == last accum then
                        removeLast accum ++ [ Maybe.map ((*) 2) cell, Nothing ]

                    else
                        accum ++ [ cell ]
                )
                []
            << addEmptyCells
            << removeEmpties
        )
        grid


removeEmpties : Row -> Row
removeEmpties =
    List.filter ((/=) Nothing)


addEmpties : Int -> Row -> Row
addEmpties size row =
    List.repeat (size - List.length row) Nothing ++ row


removeLast : Row -> Row
removeLast row =
    row
        |> List.indexedMap Tuple.pair
        |> List.filter (\( index, value ) -> index /= List.length row - 1)
        |> List.map Tuple.second


last : Row -> Maybe Int
last =
    Maybe.withDefault Nothing << List.head << List.reverse



-- STYLES


gameBoardWidth : Float
gameBoardWidth =
    500


cellSize : Float
cellSize =
    107


cellMarginSize : Float
cellMarginSize =
    15


occupiedCell : Float
occupiedCell =
    -1000

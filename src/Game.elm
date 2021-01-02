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
import Html.Attributes exposing (value)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import Json.Encode as E
import Ports exposing (storeGameState)
import Random
import StyleVariables exposing (..)
import Tile exposing (Tile, randomTiles)



-- TYPES


type Direction
    = Left
    | Right
    | Up
    | Down


type GameStatus
    = Won
    | Failed
    | InProgress


type Model
    = Internals GameState


type alias GameState =
    { bestScore : Int
    , size : Int
    , tiles : List Tile
    , score : Int
    , status : GameStatus
    , targetScore : Int
    , nextTileKey : Int
    }



-- MODEL


gridSize : Int
gridSize =
    4


initialState : ( Model, Cmd Msg )
initialState =
    ( Internals
        { bestScore = 0
        , size = gridSize
        , tiles = []
        , score = 0
        , status = InProgress
        , targetScore = 2048
        , nextTileKey = 0
        }
    , Random.generate GotNewTiles (randomTiles 2 (availableTiles gridSize []))
    )


init : D.Value -> ( Model, Cmd Msg )
init previousGS =
    let
        state =
            case D.decodeValue decode previousGS of
                Ok gs ->
                    ( Internals gs, Cmd.none )

                Err _ ->
                    initialState
    in
    state



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder



-- UPDATE


type Msg
    = KeyDown (Maybe Direction)
    | Reset
    | GotNewTiles (List Tile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Internals model) =
    case msg of
        KeyDown Nothing ->
            ( Internals model, Cmd.none )

        KeyDown (Just direction) ->
            let
                wonOrFailed =
                    model.status == Won || model.status == Failed

                newTiles =
                    if wonOrFailed then
                        model.tiles

                    else
                        move direction model.size model.tiles

                isNothingChanged =
                    wonOrFailed || List.all (\tile -> List.member tile model.tiles) newTiles

                newModel =
                    if isNothingChanged then
                        Internals model

                    else
                        Internals { model | tiles = newTiles, score = score newTiles }

                cmd =
                    if isNothingChanged then
                        Cmd.none

                    else
                        Random.generate GotNewTiles (randomTiles 1 (availableTiles model.size newTiles))
            in
            ( newModel, cmd )

        Reset ->
            initialState

        GotNewTiles newTiles ->
            let
                ( keyedTiles, nextTileKey ) =
                    Tile.withKey newTiles model.nextTileKey

                tiles =
                    withTiles keyedTiles model.tiles

                won =
                    isWon model.targetScore tiles

                failed =
                    not won && isFailed model.targetScore model.size tiles

                newModel =
                    Internals
                        { model
                            | tiles = tiles
                            , score = score tiles
                            , nextTileKey = nextTileKey
                            , status =
                                if won then
                                    Won

                                else if failed then
                                    Failed

                                else
                                    InProgress
                        }
            in
            ( newModel
            , storeGameState <| encode newModel
            )



-- VIEW


view : Model -> Html Msg
view (Internals model) =
    div
        []
        [ viewHeader model.score model.bestScore
        , viewGameContainer <| Internals model
        , viewFooter
        ]


viewHeader : Int -> Int -> Html Msg
viewHeader gScore bestScore =
    div []
        [ text "2048"
        , text <| String.fromInt gScore
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
        , Tile.view model.targetScore model.tiles
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


viewFooter : Html msg
viewFooter =
    div
        []
        [ text "Awesome game" ]



-- SERIALIZATION


keyDecoder : Decoder Msg
keyDecoder =
    D.map toDirection (D.field "key" D.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            KeyDown (Just Left)

        "ArrowRight" ->
            KeyDown (Just Right)

        "ArrowUp" ->
            KeyDown (Just Up)

        "ArrowDown" ->
            KeyDown (Just Down)

        _ ->
            KeyDown Nothing


decode : D.Decoder GameState
decode =
    D.succeed GameState
        |> DP.required "bestScore" D.int
        |> DP.required "size" D.int
        |> DP.required "tiles" (D.list Tile.decode)
        |> DP.required "score" D.int
        |> DP.required "status" statusDecoder
        |> DP.required "targetScore" D.int
        |> DP.required "nextTileKey" D.int


encode : Model -> E.Value
encode (Internals model) =
    E.object
        [ ( "bestScore", E.int model.bestScore )
        , ( "size", E.int model.size )
        , ( "tiles", E.list Tile.encode model.tiles )
        , ( "nextTileKey", E.int model.nextTileKey )
        , ( "score", E.int model.score )
        , ( "status", statusEncoder model.status )
        , ( "targetScore", E.int model.targetScore )
        ]


statusEncoder : GameStatus -> E.Value
statusEncoder status =
    E.string <|
        case status of
            Won ->
                "Won"

            Failed ->
                "Failed"

            InProgress ->
                "InProgress"


statusDecoder : D.Decoder GameStatus
statusDecoder =
    D.string
        |> D.map
            (\status ->
                case status of
                    "Won" ->
                        Won

                    "Failed" ->
                        Failed

                    "InProgress" ->
                        InProgress

                    _ ->
                        InProgress
            )



-- GETTERS


availableTiles : Int -> List Tile -> List ( Int, Int )
availableTiles size tiles =
    let
        positions : List Int
        positions =
            List.range 0 (size - 1)

        isNotIncluded : ( Int, Int ) -> Bool
        isNotIncluded ( row, column ) =
            tiles
                |> List.filter (\tile -> Tile.column tile == column && Tile.row tile == row)
                |> List.length
                |> (==) 0
    in
    cartesian positions positions
        |> List.filter isNotIncluded


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


isWon : Int -> List Tile -> Bool
isWon targetScore =
    List.any ((==) targetScore << Tile.value)


isFailed : Int -> Int -> List Tile -> Bool
isFailed targetScore size tiles =
    let
        isFull =
            List.length tiles == size * size

        sortedByRows =
            Tile.sortTilesByRows tiles

        sortedByColumns =
            Tile.sortTilesByColumns tiles

        isMovePossible : List Tile -> Bool
        isMovePossible sortedTiles =
            case sortedTiles of
                [] ->
                    False

                x :: xs ->
                    xs
                        |> List.foldr
                            (\tile { prev, sameValues } ->
                                let
                                    value =
                                        Tile.value tile

                                    row =
                                        Tile.row tile

                                    col =
                                        Tile.column tile

                                    prevValue =
                                        Tile.value prev

                                    prevRow =
                                        Tile.row prev

                                    prevCol =
                                        Tile.column prev
                                in
                                if sameValues then
                                    { prev = prev, sameValues = sameValues }

                                else if prevValue == value && (col == prevCol || row == prevRow) then
                                    { prev = tile, sameValues = True }

                                else
                                    { prev = tile, sameValues = sameValues }
                            )
                            { prev = x, sameValues = False }
                        |> .sameValues
    in
    not (isWon targetScore tiles)
        && isFull
        && (not <| isMovePossible sortedByRows)
        && (not <| isMovePossible sortedByColumns)


score : List Tile -> Int
score tiles =
    tiles
        |> List.map Tile.value
        |> List.maximum
        |> Maybe.withDefault 0



-- TRANSFORMERS


withTiles : List Tile -> List Tile -> List Tile
withTiles =
    (++)


rotateClockwise : Int -> List Tile -> List Tile
rotateClockwise size =
    Tile.reverse (size - 1) << Tile.transpose


rotateAntiClockwise : Int -> List Tile -> List Tile
rotateAntiClockwise size =
    Tile.transpose << Tile.reverse (size - 1)


move : Direction -> Int -> List Tile -> List Tile
move direction size =
    case direction of
        Right ->
            moveRight size

        Left ->
            rotateClockwise size
                << rotateClockwise size
                << moveRight size
                << rotateClockwise size
                << rotateClockwise size

        Up ->
            rotateAntiClockwise size << moveRight size << rotateClockwise size

        Down ->
            rotateClockwise size << moveRight size << rotateAntiClockwise size


moveRight : Int -> List Tile -> List Tile
moveRight size tiles =
    let
        shift =
            List.concat
                << List.map (shiftRight <| size - 1)
                << Tile.splitRows size
    in
    tiles
        |> shift
        |> List.reverse
        |> mergeRight
        |> List.reverse
        |> shift


mergeRight : List Tile -> List Tile
mergeRight tiles =
    case tiles of
        [] ->
            []

        x :: [] ->
            [ x ]

        x :: y :: xs ->
            if Tile.value x == Tile.value y && Tile.row x == Tile.row y then
                Tile.withValue (Tile.value x * 2) y :: mergeRight xs

            else
                x :: mergeRight (y :: xs)


shiftRight : Int -> List Tile -> List Tile
shiftRight maxColumn =
    List.reverse << List.indexedMap (\index -> Tile.moveRight (maxColumn - index)) << List.reverse

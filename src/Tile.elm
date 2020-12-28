module Tile exposing (Tile, column, randomTiles, row, value, view, withKey)

import Css exposing (..)
import Css.Transitions
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed
import Random as R
import StyleVariables exposing (..)



-- TYPES


type Tile
    = Tile String Internals


type alias Internals =
    { row : Int
    , column : Int
    , value : Int
    }



-- GETTERS


column : Tile -> Int
column (Tile _ internals) =
    internals.column


row : Tile -> Int
row (Tile _ internals) =
    internals.row


value : Tile -> Int
value (Tile _ internals) =
    internals.value



-- TRANSFORM


withKey : List Tile -> Int -> ( List Tile, Int )
withKey tiles initialKey =
    let
        newInitialKey =
            initialKey + List.length tiles

        gotKey index =
            String.fromInt <| initialKey + index

        tileWithKey index (Tile _ internals) =
            Tile (gotKey index) internals
    in
    ( List.indexedMap tileWithKey tiles
    , newInitialKey
    )



-- GENERATORS


randomValue : R.Generator Int
randomValue =
    R.weighted ( 80, 2 ) [ ( 20, 4 ) ]


randomPosition : List ( Int, Int ) -> R.Generator ( Int, Int )
randomPosition availablePositions =
    case availablePositions of
        [] ->
            R.constant ( 0, 0 )

        first :: rest ->
            R.uniform first rest


randomTile : List ( Int, Int ) -> R.Generator Internals
randomTile availablePositions =
    R.map2 (\( rowIndex, colIndex ) -> Internals rowIndex colIndex) (randomPosition availablePositions) randomValue


randomTiles : Int -> List ( Int, Int ) -> R.Generator (List Tile)
randomTiles count availablePositions =
    randomTile availablePositions
        |> R.list count
        |> R.map (List.map <| Tile "HARDCODED_KEY")



-- VIEW


view : List Tile -> Html msg
view tiles =
    Keyed.node "div"
        [ css
            [ position absolute
            , zIndex <| int 2
            ]
        ]
        (List.map viewKeyedTile tiles)


viewKeyedTile : Tile -> ( String, Html msg )
viewKeyedTile tile =
    let
        (Tile key _) =
            tile
    in
    ( key, viewTile tile )


viewTile : Tile -> Html msg
viewTile (Tile _ internals) =
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
            , transform <|
                translate2
                    (px (toFloat internals.column * wholeCellSize))
                    (px (toFloat internals.row * wholeCellSize))
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
            [ (text << String.fromInt) internals.value ]
        ]

module StyleVariables exposing
    ( cellMarginSize
    , cellSize
    , gameBoardWidth
    , mix
    , occupiedCell
    )

import Hex


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


tileBgInitialColor : String
tileBgInitialColor =
    "f4e4d4"


tileBgTargetColor : String
tileBgTargetColor =
    "fecb1d"


getRGB : String -> ( Int, Int, Int )
getRGB color =
    ( Result.withDefault 0 <| Hex.fromString <| String.left 2 color
    , Result.withDefault 0 <| Hex.fromString <| String.left 2 <| String.dropLeft 2 color
    , Result.withDefault 0 <| Hex.fromString <| String.left 2 <| String.dropLeft 4 color
    )


mix : Float -> String
mix weight =
    let
        ( iR, iG, iB ) =
            getRGB tileBgInitialColor

        ( tR, tG, tB ) =
            getRGB tileBgTargetColor

        ( r, g, b ) =
            ( floor <| toFloat iR + toFloat (tR - iR) * weight / 100
            , floor <| toFloat iG + toFloat (tG - iG) * weight / 100
            , floor <| toFloat iB + toFloat (tB - iB) * weight / 100
            )
    in
    [ r, g, b ]
        |> List.map Hex.toString
        |> String.concat

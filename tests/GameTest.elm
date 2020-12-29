module GameTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game exposing (Direction(..), move)
import Test exposing (..)
import Tile exposing (Tile(..))


listsEqualWithOrderIgnoring : List Tile -> List Tile -> Expectation
listsEqualWithOrderIgnoring expected resulted =
    Expect.true
        "resulted tiles should be the same as expected"
        (List.all (\tile -> List.member tile expected) <| resulted)


suite : Test
suite =
    describe "grid recalculation on move"
        [ test "move right (without rotations) 2 * 2"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 2, row = 0, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 1, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 1, column = 1, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 2, row = 0, column = 1, merged = False }
                        , Tile "key" { value = 4, row = 1, column = 1, merged = False }
                        ]
                in
                move Right 2 tiles
                    |> listsEqualWithOrderIgnoring expected
            )
        , test "move right (without rotations) 3 * 3"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 2, row = 0, column = 1, merged = False }
                        , Tile "key" { value = 2, row = 2, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 2, column = 1, merged = False }
                        , Tile "key" { value = 8, row = 2, column = 2, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 2, row = 0, column = 2, merged = False }
                        , Tile "key" { value = 4, row = 2, column = 1, merged = False }
                        , Tile "key" { value = 8, row = 2, column = 2, merged = False }
                        ]
                in
                move Right 3 tiles
                    |> listsEqualWithOrderIgnoring expected
            )
        , test "move right (without rotations) 3 * 3 - (2 - 2 - 2)"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 2, row = 2, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 2, column = 1, merged = False }
                        , Tile "key" { value = 2, row = 2, column = 2, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 2, row = 2, column = 1, merged = False }
                        , Tile "key" { value = 4, row = 2, column = 2, merged = False }
                        ]
                in
                move Right 3 tiles
                    |> listsEqualWithOrderIgnoring expected
            )
        , test "move right (without rotations) 4 * 4"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 2, row = 0, column = 2, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 1, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 2, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 3, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 2, row = 0, column = 3, merged = False }
                        , Tile "key" { value = 4, row = 3, column = 2, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 3, merged = False }
                        ]
                in
                move Right 4 tiles
                    |> listsEqualWithOrderIgnoring expected
            )
        , test "move right (without rotations) 4 * 4 (- 4 - 4 | - - 2 8)"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 4, row = 2, column = 1, merged = False }
                        , Tile "key" { value = 4, row = 2, column = 3, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 2, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 3, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 8, row = 2, column = 3, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 2, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 3, merged = False }
                        ]
                in
                move Right 4 tiles
                    |> listsEqualWithOrderIgnoring expected
            )
        , test "move down 4 * 4"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 2, row = 1, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 2, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 2, column = 3, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 0, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 4, row = 2, column = 0, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 3, merged = False }
                        ]
                in
                move Down 4 tiles
                    |> listsEqualWithOrderIgnoring expected
            )
        ]

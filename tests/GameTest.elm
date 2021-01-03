module GameTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game exposing (Direction(..), isFailed, move)
import Test exposing (..)
import Tile exposing (Tile(..))


listsEqualWithOrderIgnoring : List Tile -> List Tile -> Expectation
listsEqualWithOrderIgnoring expected resulted =
    Expect.true
        "resulted tiles should be the same as expected"
        (List.all (\tile -> List.member tile expected) <| resulted)


suite : Test
suite =
    describe "Game Module"
        [ describe "grid recalculation on move"
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
            , test "move left for almost failed game"
                (\() ->
                    let
                        tiles =
                            [ Tile "56" { row = 0, column = 0, merged = False, value = 2 }
                            , Tile "52" { row = 0, column = 1, merged = False, value = 2 }
                            , Tile "55" { row = 0, column = 2, merged = False, value = 4 }
                            , Tile "21" { row = 1, column = 0, merged = False, value = 64 }
                            , Tile "54" { row = 1, column = 1, merged = False, value = 4 }
                            , Tile "47" { row = 1, column = 2, merged = False, value = 8 }
                            , Tile "50" { row = 2, column = 0, merged = False, value = 2 }
                            , Tile "49" { row = 2, column = 1, merged = False, value = 16 }
                            , Tile "36" { row = 2, column = 2, merged = False, value = 32 }
                            ]

                        expected =
                            [ Tile "52" { row = 0, column = 0, merged = False, value = 4 }
                            , Tile "55" { row = 0, column = 1, merged = False, value = 4 }
                            , Tile "21" { row = 1, column = 0, merged = False, value = 64 }
                            , Tile "54" { row = 1, column = 1, merged = False, value = 4 }
                            , Tile "47" { row = 1, column = 2, merged = False, value = 8 }
                            , Tile "50" { row = 2, column = 0, merged = False, value = 2 }
                            , Tile "49" { row = 2, column = 1, merged = False, value = 16 }
                            , Tile "36" { row = 2, column = 2, merged = False, value = 32 }
                            ]
                    in
                    move Left 3 tiles
                        |> listsEqualWithOrderIgnoring expected
                )
            ]
        , describe "check for failure"
            [ test "check for failure 3 * 3"
                (\() ->
                    let
                        tiles =
                            [ Tile "56" { row = 0, column = 0, merged = False, value = 2 }
                            , Tile "52" { row = 0, column = 1, merged = False, value = 2 }
                            , Tile "55" { row = 0, column = 2, merged = False, value = 4 }
                            , Tile "21" { row = 1, column = 0, merged = False, value = 64 }
                            , Tile "54" { row = 1, column = 1, merged = False, value = 4 }
                            , Tile "47" { row = 1, column = 2, merged = False, value = 8 }
                            , Tile "50" { row = 2, column = 0, merged = False, value = 2 }
                            , Tile "49" { row = 2, column = 1, merged = False, value = 16 }
                            , Tile "36" { row = 2, column = 2, merged = False, value = 32 }
                            ]
                    in
                    Expect.false "there is at least one move" <| isFailed 2048 3 tiles
                )
            ]
        ]

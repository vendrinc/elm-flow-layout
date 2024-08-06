module Unit.Flow.Line exposing (suite)

import Expect
import Flow.Line exposing (Path(..), Point)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Flow.Line"
        [ describe "toCommands"
            [ horizontal
            , vertical
            ]
        ]


horizontal : Test
horizontal =
    describe "horizontal"
        [ test "same y, no middle points" <|
            \_ ->
                Horizontal (Point 2 0) [] (Point 6 0)
                    |> Flow.Line.toCommands 5
                    {-
                       Start ──── End
                    -}
                    |> Expect.equal "M 2,0 L 6,0"
        , test "same y, with middle points" <|
            \_ ->
                Horizontal (Point 2 0) [ Point 4 0 ] (Point 6 0)
                    |> Flow.Line.toCommands 5
                    {-
                                Middle
                       Start ──────────── End
                    -}
                    |> Expect.equal "M 2,0 L 4,0 L 6,0"
        , test "left-down to right-up, no middle" <|
            \_ ->
                Horizontal (Point 10 20) [] (Point 30 0)
                    |> Flow.Line.toCommands 5
                    {-
                                   ╭ End
                                   │
                                   │
                       Start ──────╯

                    -}
                    |> Expect.equal "M 10,20 L 25,20 S 30,20 30,15 L 30,5 S 30,0 35,0"
        , test "left-up to right-down, no middle" <|
            \_ ->
                Horizontal (Point 10 0) [] (Point 30 20)
                    |> Flow.Line.toCommands 5
                    {-
                       Start ──────╮
                                   │
                                   │
                                   ╰ End

                    -}
                    |> Expect.equal "M 10,0 L 25,0 S 30,0 30,5 L 30,15 S 30,20 35,20"
        , test "left-down to right-up with middle" <|
            \_ ->
                Horizontal (Point 10 20) [ Point 20 0 ] (Point 30 0)
                    |> Flow.Line.toCommands 5
                    {-

                                Middle
                                   ╭────── End
                                   │
                                   │
                       Start ──────╯

                    -}
                    |> Expect.equal "M 10,20 L 15,20 S 20,20 20,15 L 20,5 S 20,0 25,0 L 30,0"
        , test "left-up to right-down with middle" <|
            \_ ->
                Horizontal (Point 10 0) [ Point 20 20 ] (Point 30 20)
                    |> Flow.Line.toCommands 5
                    {-
                       Start ──────╮
                                   │
                                   │
                                   ╰────── End
                                Middle

                    -}
                    |> Expect.equal "M 10,0 L 15,0 S 20,0 20,5 L 20,15 S 20,20 25,20 L 30,20"
        , test "left-down to right-up with multiple middle points" <|
            \_ ->
                Horizontal (Point 10 20) [ Point 20 0, Point 25 0 ] (Point 30 15)
                    |> Flow.Line.toCommands 5
                    {-
                            Middle 1   2
                                   ╭───╮
                                   │   │
                                   │   ╰──── End
                                   │
                       Start ──────╯

                    -}
                    |> Expect.equal "M 10,20 L 15,20 S 20,20 20,15 L 20,5 S 20,0 25,0 L 25,0 L 25,0 S 30,0 30,5 L 30,10 S 30,15 35,15"
        , test "left-up to right-down with multiple middle points" <|
            \_ ->
                Horizontal (Point 10 0) [ Point 20 10, Point 25 10 ] (Point 30 5)
                    |> Flow.Line.toCommands 5
                    {-
                       Start ──────╮
                                   │
                                   │   ╭──── End
                                   │   │
                                   ╰───╯
                            Middle 1   2

                    -}
                    |> Expect.equal "M 10,0 L 15,0 S 20,0 20,5 L 20,5 S 20,10 25,10 L 25,10 L 25,10 S 30,10 30,5 L 30,10 S 30,5 35,5"
        ]


vertical : Test
vertical =
    describe "vertical"
        [ test "same x" <|
            \_ ->
                Vertical (Point 0 2) (Point 0 6)
                    |> Flow.Line.toCommands 5
                    {-
                       Start
                         │
                         │
                        End
                    -}
                    |> Expect.equal "M 0,2 L 0,6"
        , test "left-down to right-up" <|
            \_ ->
                Vertical (Point 10 20) (Point 30 0)
                    |> Flow.Line.toCommands 5
                    {-
                                  End
                                   │
                                   │
                       Start ──────╯

                    -}
                    |> Expect.equal "M 10,20 L 25,20 S 30,20 30,15 L 30,0"
        , test "left-up to right-down" <|
            \_ ->
                Vertical (Point 10 0) (Point 30 20)
                    |> Flow.Line.toCommands 5
                    {-
                       Start ──────╮
                                   │
                                   │
                                  End

                    -}
                    |> Expect.equal "M 10,0 L 25,0 S 30,0 30,5 L 30,20"
        , test "right-up to left-down" <|
            \_ ->
                Vertical (Point 20 0) (Point 0 30)
                    |> Flow.Line.toCommands 5
                    {-
                            Start ─╮
                                   │
                        ╭──────────╯
                        │
                       End

                    -}
                    |> Expect.equal "M 20,0 L 22,0 S 27,0 27,5 L 27,10 S 27,15 22,15 L 5,15 S 0,15 0,20 L 0,30"
        , test "right-down to left-up" <|
            \_ ->
                Vertical (Point 20 20) (Point 0 0)
                    |> Flow.Line.toCommands 5
                    {-
                       End
                        │
                        ╰─────────╮
                                  │
                           Start ─╯

                    -}
                    |> Expect.equal "M 20,20 L 22,20 S 27,20 27,15 L 27,15 S 27,10 22,10 L 5,10 S 0,10 0,5 L 0,0"
        ]

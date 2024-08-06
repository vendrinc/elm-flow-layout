module Unit.Flow.Layout exposing (suite)

import Expect
import Flow exposing (Flow(..))
import Flow.Layout exposing (Box, ConnectionEnding(..), Layout, Pos)
import Flow.Line exposing (Path(..))
import Fuzz exposing (Fuzzer)
import Test exposing (Test, test)
import Unit.Flow exposing (flowFuzzer)


suite : Test
suite =
    Test.describe "Flow.Layout"
        [ singleNode
        , sequential
        , parallel
        , condition
        , nested
        , properties
        ]


singleNode : Test
singleNode =
    Test.describe "single node"
        [ test "positioned and sized correctly" <|
            \() ->
                Node "A"
                    |> Flow.Layout.layout (Pos 10 15)
                        { nodeProperties = \_ -> { width = 30, height = 20, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 2
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 5
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal [ ( "A", Box 10 15 30 20 2 10 ) ]
        , test "margin moves the box and expands the layout" <|
            \() ->
                Node "A"
                    |> Flow.Layout.layout (Pos 10 15)
                        { nodeProperties = \_ -> { width = 30, height = 20, marginLeft = 5, marginRight = 10 }
                        , connectionY = always 2
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 5
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal [ ( "A", Box 15 15 30 20 2 10 ) ]
        , test "no connections produced" <|
            \() ->
                Node "A"
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal []
        , test "no condition roots" <|
            \() ->
                Node "A"
                    |> defaultLayout
                    |> .conditionRoots
                    |> Expect.equal []
        , test "leftmost box is node" <|
            \() ->
                Node "A"
                    |> defaultLayout
                    |> .left
                    |> Expect.equal [ ( CNode "A", Box 0 0 10 10 5 0 ) ]
        , test "rightmost box is node" <|
            \() ->
                Node "A"
                    |> defaultLayout
                    |> .left
                    |> Expect.equal [ ( CNode "A", Box 0 0 10 10 5 0 ) ]
        ]


sequential : Test
sequential =
    Test.describe "sequential nodes"
        [ test "nodes are separated by x gap" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "B", Box 15 0 10 10 5 15 )
                        , ( "C", Box 30 0 10 10 5 30 )
                        ]
        , test "gap respects node margins" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties =
                            \node ->
                                case node of
                                    "A" ->
                                        { width = 10, height = 10, marginLeft = 0, marginRight = 5 }

                                    "B" ->
                                        { width = 10, height = 10, marginLeft = 5, marginRight = 0 }

                                    _ ->
                                        { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 5
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "B", Box 25 0 10 10 5 20 )
                        , ( "C", Box 40 0 10 10 5 40 )
                        ]
        , test "each node connects to previous one" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "B", Horizontal (Pos 15 5) [ Pos 28 5 ] (Pos 30 5), CNode "C" )
                        , ( CNode "A", Horizontal (Pos 0 5) [ Pos 13 5 ] (Pos 15 5), CNode "B" )
                        ]
        , test "connections to node with margin" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties =
                            \node ->
                                case node of
                                    "A" ->
                                        { width = 10, height = 10, marginLeft = 0, marginRight = 5 }

                                    "B" ->
                                        { width = 10, height = 10, marginLeft = 5, marginRight = 0 }

                                    _ ->
                                        { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 5
                        , isConditionRoot = \_ -> True
                        }
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "B", Horizontal (Pos 25 5) [ Pos 38 5 ] (Pos 40 5), CNode "C" )
                        , ( CNode "A", Horizontal (Pos 0 5) [ Pos 18 5 ] (Pos 25 5), CNode "B" )
                        ]
        , test "leftmost box is first node" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .left
                    |> List.map Tuple.first
                    |> Expect.equal [ CNode "A" ]
        , test "rightmost box is last node" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .right
                    |> List.map Tuple.first
                    |> Expect.equal [ CNode "C" ]
        ]


parallel : Test
parallel =
    Test.describe "parallel nodes"
        [ test "nodes are separated by y gap" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "B", Box 0 15 10 10 5 0 )
                        , ( "C", Box 0 30 10 10 5 0 )
                        ]
        , test "condition is separated by conditionBranchGapY" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Condition "?"
                        { trueSeq = [ Node "True" ]
                        , falseSeq = [ Node "False" ]
                        }
                    ]
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 15
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "?", Box 0 25 10 10 5 0 ) -- ?.top - A.bottom == conditionBranchGapY
                        , ( "True", Box 15 25 10 10 5 15 )
                        , ( "False", Box 15 50 10 10 5 15 )
                        ]
        , test "sequence containing a condition in the top row is separated by conditionBranchGapY" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Sequential
                        [ Node "B"
                        , Condition "?"
                            { trueSeq = [ Node "True" ]
                            , falseSeq = [ Node "False" ]
                            }
                        ]
                    ]
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 15
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "B", Box 0 25 10 10 5 0 ) -- B.top - A.bottom == conditionBranchGapY
                        , ( "?", Box 15 25 10 10 5 15 )
                        , ( "True", Box 30 25 10 10 5 30 )
                        , ( "False", Box 30 50 10 10 5 30 )
                        ]
        , test "sequence containing a condition in a the first row of a nested parallel is separated by conditionBranchGapY" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Sequential
                        [ Node "B"
                        , Parallel
                            [ Condition "?"
                                { trueSeq = [ Node "True" ]
                                , falseSeq = [ Node "False" ]
                                }
                            , Node "C"
                            ]
                        ]
                    ]
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 15
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "B", Box 0 25 10 10 5 0 ) -- B.top - A.bottom == conditionBranchGapY
                        , ( "?", Box 15 25 10 10 5 15 )
                        , ( "True", Box 30 25 10 10 5 30 )
                        , ( "False", Box 30 50 10 10 5 30 )
                        , ( "C", Box 15 65 10 10 5 15 )
                        ]
        , test "sequence containing a condition in a later row is separated by regular y gap" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Sequential
                        [ Node "B"
                        , Parallel
                            [ Node "C"
                            , Condition "?"
                                { trueSeq = [ Node "True" ]
                                , falseSeq = [ Node "False" ]
                                }
                            ]
                        ]
                    ]
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 15
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "B", Box 0 15 10 10 5 0 ) -- B.top - A.bottom == gapY
                        , ( "C", Box 15 15 10 10 5 15 )
                        , ( "?", Box 15 40 10 10 5 15 )
                        , ( "True", Box 30 40 10 10 5 30 )
                        , ( "False", Box 30 65 10 10 5 30 )
                        ]
        , test "no nodes are connected" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal []
        , test "leftmost boxes are all nodes" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .left
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ CNode "C"
                        , CNode "B"
                        , CNode "A"
                        ]
        , test "rightmost boxes are all nodes" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Node "B"
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .right
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ CNode "C"
                        , CNode "B"
                        , CNode "A"
                        ]
        ]


condition : Test
condition =
    let
        conditionLayout : Layout String
        conditionLayout =
            Condition "?"
                { trueSeq = [ Node "True 1", Node "True 2" ]
                , falseSeq = [ Node "False" ]
                }
                |> defaultLayout
    in
    Test.describe "condition"
        [ test "condition node positioned on the left of true sequence which is on top of false sequence" <|
            \() ->
                conditionLayout
                    |> .nodes
                    |> Expect.equal
                        [ ( "?", Box 0 0 10 10 5 0 )
                        , ( "True 1", Box 15 0 10 10 5 15 )
                        , ( "True 2", Box 30 0 10 10 5 30 )
                        , ( "False", Box 15 15 10 10 5 15 )
                        ]
        , test "sequences are separated from condition node by conditionBranchGapX" <|
            \() ->
                Condition "?"
                    { trueSeq = [ Node "True" ]
                    , falseSeq = [ Node "False" ]
                    }
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 20
                        , conditionBranchGapY = 5
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal
                        [ ( "?", Box 0 0 10 10 5 0 )
                        , ( "True", Box 30 0 10 10 5 30 )
                        , ( "False", Box 30 15 10 10 5 30 )
                        ]
        , test "sequence position respects leftmost node's marginLeft" <|
            \() ->
                Condition "?"
                    { trueSeq = [ Node "True" ]
                    , falseSeq = [ Node "False" ]
                    }
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties =
                            \node ->
                                if node == "True" then
                                    { width = 10, height = 10, marginLeft = 5, marginRight = 0 }

                                else
                                    { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 20
                        , conditionBranchGapY = 5
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal
                        [ ( "?", Box 0 0 10 10 5 0 )
                        , ( "True", Box 35 0 10 10 5 30 )
                        , ( "False", Box 30 15 10 10 5 30 )
                        ]
        , test "true seq is separated from false seq by conditionBranchGapY" <|
            \() ->
                Condition "?"
                    { trueSeq = [ Node "True" ]
                    , falseSeq = [ Node "False" ]
                    }
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 15
                        , isConditionRoot = \_ -> True
                        }
                    |> .nodes
                    |> Expect.equal
                        [ ( "?", Box 0 0 10 10 5 0 )
                        , ( "True", Box 15 0 10 10 5 15 )
                        , ( "False", Box 15 25 10 10 5 15 )
                        ]
        , test "sequences are connected to condition node" <|
            \() ->
                conditionLayout
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "?", Horizontal (Pos 0 5) [ Pos 13 5 ] (Pos 15 5), CNode "True 1" )
                        , ( CNode "?", Horizontal (Pos 0 5) [ Pos 13 20 ] (Pos 15 20), CNode "False" )
                        , ( CNode "True 1", Horizontal (Pos 15 5) [ Pos 28 5 ] (Pos 30 5), CNode "True 2" )
                        ]
        , test "parallel branch endings" <|
            \() ->
                Condition "?"
                    { trueSeq = [ Node "True 1", Node "True 2" ]
                    , falseSeq =
                        [ Parallel
                            [ Node "False"
                            , Node "False 2"
                            ]
                        ]
                    }
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "?", Horizontal (Pos 0 5) [ Pos 13 5 ] (Pos 15 5), BranchStart )
                        , ( CNode "?", Horizontal (Pos 0 5) [ Pos 13 20 ] (Pos 15 20), BranchStart )
                        , ( BranchStart, Horizontal (Pos 15 5) [ Pos 18 5 ] (Pos 20 5), CNode "True 1" )
                        , ( CNode "True 2", Horizontal (Pos 35 5) [ Pos 48 5 ] (Pos 50 5), BranchEnd )
                        , ( CNode "True 1", Horizontal (Pos 20 5) [ Pos 33 5 ] (Pos 35 5), CNode "True 2" )
                        , ( BranchStart, Horizontal (Pos 15 20) [ Pos 18 35 ] (Pos 20 35), CNode "False 2" )
                        , ( BranchStart, Horizontal (Pos 15 20) [ Pos 18 20 ] (Pos 20 20), CNode "False" )
                        , ( CNode "False 2", Horizontal (Pos 20 35) [ Pos 33 20 ] (Pos 35 20), BranchEnd )
                        , ( CNode "False", Horizontal (Pos 20 20) [ Pos 33 20 ] (Pos 35 20), BranchEnd )
                        ]
        , test "condition root is first node in seqs that match `isConditionRoot`" <|
            \() ->
                Condition 0
                    { trueSeq = [ Node 1, Node 2 ]
                    , falseSeq = [ Node 3, Node 4 ]
                    }
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 5
                        , isConditionRoot = \n -> modBy 2 n == 0
                        }
                    |> .conditionRoots
                    |> Expect.equal
                        [ { box = Box 30 0 10 10 5 30, branch = True, condition = 0 }
                        , { box = Box 30 15 10 10 5 30, branch = False, condition = 0 }
                        ]
        , test "condition root is first node in seq if none match `isConditionRoot`" <|
            \() ->
                Condition 0
                    { trueSeq = [ Node 1, Node 2 ]
                    , falseSeq = [ Node 3 ]
                    }
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
                        , connectionY = always 5
                        , gapX = 5
                        , gapY = 5
                        , conditionBranchGapX = 5
                        , conditionBranchGapY = 5
                        , isConditionRoot = \n -> modBy 2 n == 0
                        }
                    |> .conditionRoots
                    |> Expect.equal
                        [ { box = Box 30 0 10 10 5 30, branch = True, condition = 0 }
                        , { box = Box 15 15 10 10 5 15, branch = False, condition = 0 }
                        ]
        , test "leftmost box is condition node" <|
            \() ->
                conditionLayout
                    |> .left
                    |> List.map Tuple.first
                    |> Expect.equal [ CNode "?" ]
        , test "rightmost boxes are true rightmost ++ false rightmost" <|
            \() ->
                conditionLayout
                    |> .right
                    |> List.map Tuple.first
                    |> Expect.equal [ CNode "True 2", CNode "False" ]
        ]


nested : Test
nested =
    Test.describe "nested"
        [ test "parallel inside sequential positions" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Parallel
                        [ Node "B"
                        , Node "C"
                        ]
                    ]
                    |> defaultLayout
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "B", Box 15 0 10 10 5 15 )
                        , ( "C", Box 15 15 10 10 5 15 )
                        ]
        , test "leftmost node of each track connect to preceding node" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Parallel
                        [ Sequential [ Node "B", Node "C" ]
                        , Node "D"
                        ]
                    ]
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "A", Horizontal (Pos 0 5) [ Pos 13 20 ] (Pos 15 20), CNode "D" )
                        , ( CNode "A", Horizontal (Pos 0 5) [ Pos 13 5 ] (Pos 15 5), CNode "B" )
                        , ( CNode "B", Horizontal (Pos 15 5) [ Pos 28 5 ] (Pos 30 5), CNode "C" )
                        ]
        , test "subsequent node connects to rightmost node of each previous parallel track" <|
            \() ->
                Sequential
                    [ Parallel
                        [ Node "A"
                        , Node "B"
                        ]
                    , Node "C"
                    ]
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "B", Horizontal (Pos 0 20) [ Pos 13 5 ] (Pos 15 5), CNode "C" )
                        , ( CNode "A", Horizontal (Pos 0 5) [ Pos 13 5 ] (Pos 15 5), CNode "C" )
                        ]
        , test "subsequent node positioned after longest parallel track" <|
            \() ->
                Sequential
                    [ Parallel
                        [ Sequential [ Node "A", Node "B" ]
                        , Node "C"
                        ]
                    , Node "D"
                    ]
                    |> defaultLayout
                    |> .nodes
                    |> Expect.equal
                        [ ( "A", Box 0 0 10 10 5 0 )
                        , ( "B", Box 15 0 10 10 5 15 )
                        , ( "C", Box 0 15 10 10 5 0 )
                        , ( "D", Box 30 0 10 10 5 30 )
                        ]
        , test "all connections from subsequent node to parallel tracks turn at the same x position" <|
            {-
               A ─── B ─── C ──╭─ G
               D ─── E ────────┘
               F ──────────────╯

            -}
            \() ->
                Sequential
                    [ Parallel
                        [ Sequential [ Node "A", Node "B", Node "C" ]
                        , Sequential [ Node "D", Node "E" ]
                        , Node "F"
                        ]
                    , Node "G"
                    ]
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "F", Horizontal (Pos 0 35) [ Pos 43 5 ] (Pos 45 5), CNode "G" )
                        , ( CNode "E", Horizontal (Pos 15 20) [ Pos 43 5 ] (Pos 45 5), CNode "G" )
                        , ( CNode "C", Horizontal (Pos 30 5) [ Pos 43 5 ] (Pos 45 5), CNode "G" )
                        , ( CNode "D", Horizontal (Pos 0 20) [ Pos 13 20 ] (Pos 15 20), CNode "E" )
                        , ( CNode "B", Horizontal (Pos 15 5) [ Pos 28 5 ] (Pos 30 5), CNode "C" )
                        , ( CNode "A", Horizontal (Pos 0 5) [ Pos 13 5 ] (Pos 15 5), CNode "B" )
                        ]
        , test "back to back parallels should have a step in the middle" <|
            \() ->
                Sequential
                    [ Parallel [ Node "A", Node "B" ]
                    , Parallel [ Node "C", Node "D" ]
                    ]
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "B", Horizontal (Pos 0 20) [ Pos 12 5, Pos 14 20 ] (Pos 15 20), CNode "D" )
                        , ( CNode "A", Horizontal (Pos 0 5) [ Pos 12 5, Pos 14 20 ] (Pos 15 20), CNode "D" )
                        , ( CNode "B", Horizontal (Pos 0 20) [ Pos 12 5, Pos 14 5 ] (Pos 15 5), CNode "C" )
                        , ( CNode "A", Horizontal (Pos 0 5) [ Pos 12 5, Pos 14 5 ] (Pos 15 5), CNode "C" )
                        ]
        , test "parallels after condition should have a step in the middle" <|
            \() ->
                Sequential
                    [ Condition "?"
                        { trueSeq = [ Node "A" ]
                        , falseSeq = [ Node "B" ]
                        }
                    , Parallel [ Node "C", Node "D" ]
                    ]
                    |> defaultLayout
                    |> .connections
                    |> Expect.equal
                        [ ( CNode "A", Horizontal (Pos 15 5) [ Pos 27 5, Pos 29 20 ] (Pos 30 20), CNode "D" )
                        , ( CNode "B", Horizontal (Pos 15 20) [ Pos 27 5, Pos 29 20 ] (Pos 30 20), CNode "D" )
                        , ( CNode "A", Horizontal (Pos 15 5) [ Pos 27 5, Pos 29 5 ] (Pos 30 5), CNode "C" )
                        , ( CNode "B", Horizontal (Pos 15 20) [ Pos 27 5, Pos 29 5 ] (Pos 30 5), CNode "C" )
                        , ( CNode "?", Horizontal (Pos 0 5) [ Pos 13 5 ] (Pos 15 5), CNode "A" )
                        , ( CNode "?", Horizontal (Pos 0 5) [ Pos 13 20 ] (Pos 15 20), CNode "B" )
                        ]
        ]


properties : Test
properties =
    Test.describe "properties"
        [ Test.fuzz layoutFuzzer "boxes should never overlap" <|
            \( layout, _ ) ->
                layout.nodes
                    |> List.map Tuple.second
                    |> filterComparingToOther boxesOverlap
                    |> Expect.equal []
        , Test.fuzz layoutFuzzer "marginBoxX is always less than or equal to x" <|
            \( layout, _ ) ->
                layout.nodes
                    |> List.filter (\( _, box ) -> box.marginBoxX <= box.x)
                    |> Expect.equal layout.nodes
        , Test.fuzz layoutFuzzer "connections should never intersect (except collinearly)" <|
            \( layout, _ ) ->
                layout.connections
                    |> filterComparingToOther connectionsIntersect
                    |> Expect.equal []
        , Test.test "connections should never intersect (except collinearly): found case 1" <|
            \() ->
                Sequential
                    [ Parallel
                        [ Node "1"
                        , Node "2"
                        , Condition "?3"
                            { trueSeq = []
                            , falseSeq = [ Node "4" ]
                            }
                        , Node "5"
                        ]
                    , Node "6"
                    ]
                    |> Flow.Layout.layout (Pos 0 0)
                        { nodeProperties = \_ -> { width = 1, height = 1, marginLeft = 4, marginRight = 0 }
                        , connectionY = always 0
                        , gapX = 0
                        , gapY = 0
                        , conditionBranchGapX = 0
                        , conditionBranchGapY = 0
                        , isConditionRoot = \_ -> True
                        }
                    |> .connections
                    |> filterComparingToOther connectionsIntersect
                    |> Expect.equal []
        , Test.fuzz layoutFuzzer "nodes match descendants order" <|
            \( layout, flow ) ->
                layout.nodes
                    |> List.map Tuple.first
                    |> Expect.equal
                        (flow
                            |> Flow.descendants
                            |> List.filterMap
                                (\subFlow ->
                                    case subFlow of
                                        Node node ->
                                            Just node

                                        Condition node _ ->
                                            Just node

                                        _ ->
                                            Nothing
                                )
                        )
        , Test.test "pathToSegments" <|
            \() ->
                Horizontal (Pos 1 2) [ Pos 3 4 ] (Pos 5 6)
                    |> pathToSegments
                    |> Expect.equal
                        [ ( Pos 1 2, Pos 3 2 )
                        , ( Pos 3 2, Pos 3 4 )
                        , ( Pos 3 4, Pos 5 4 )
                        , ( Pos 5 4, Pos 5 6 )
                        ]
        ]


boxesOverlap : Box -> Box -> Bool
boxesOverlap a b =
    let
        rightA =
            a.x + a.width

        bottomA =
            a.y + a.height

        rightB =
            b.x + b.width

        bottomB =
            b.y + b.height
    in
    not (a.x >= rightB || b.x >= rightA || a.y >= bottomB || b.y >= bottomA)


connectionsIntersect : ( n, Path, n ) -> ( n, Path, n ) -> Bool
connectionsIntersect ( _, a, _ ) ( _, b, _ ) =
    let
        aSegments =
            pathToSegments a

        bSegments =
            pathToSegments b
    in
    List.any (\segA -> List.any (segmentsIntersect segA) bSegments) aSegments


type alias Segment =
    ( Pos, Pos )


segmentsIntersect : Segment -> Segment -> Bool
segmentsIntersect ( p1, p2 ) ( p3, p4 ) =
    -- Ignoring collinearity as that is valid in layouts
    let
        d1 =
            direction p3 p4 p1

        d2 =
            direction p3 p4 p2

        d3 =
            direction p1 p2 p3

        d4 =
            direction p1 p2 p4
    in
    ((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0))


direction : Pos -> Pos -> Pos -> Int
direction p q r =
    (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)


pathToSegments : Path -> List Segment
pathToSegments path =
    let
        points : List Pos
        points =
            case path of
                Horizontal start middle end ->
                    start :: middle ++ [ end ]

                Vertical start end ->
                    [ start, end ]
    in
    segmentHelp points []
        |> List.reverse


segmentHelp : List Pos -> List Segment -> List Segment
segmentHelp points gathered =
    case points of
        one :: two :: rest ->
            let
                turn : Pos
                turn =
                    { x = two.x
                    , y = one.y
                    }

                new : List Segment
                new =
                    ( turn, two )
                        :: ( one, turn )
                        :: gathered
            in
            segmentHelp (two :: rest) new

        _ ->
            gathered


filterComparingToOther : (a -> a -> Bool) -> List a -> List ( a, a )
filterComparingToOther compare items =
    let
        indexed =
            List.indexedMap Tuple.pair items
    in
    List.concatMap
        (\( idA, a ) ->
            List.filterMap
                (\( idB, b ) ->
                    if idA /= idB && compare a b then
                        Just ( a, b )

                    else
                        Nothing
                )
                indexed
        )
        indexed


defaultLayout : Flow a -> Layout a
defaultLayout =
    Flow.Layout.layout (Pos 0 0)
        { nodeProperties = \_ -> { width = 10, height = 10, marginLeft = 0, marginRight = 0 }
        , connectionY = always 5
        , gapX = 5
        , gapY = 5
        , conditionBranchGapX = 5
        , conditionBranchGapY = 5
        , isConditionRoot = \_ -> True
        }



-- FUZZERS


layoutFuzzer : Fuzzer ( Layout Int, Flow Int )
layoutFuzzer =
    Fuzz.constant
        (\x y w h ml mr nodeConnectionY gapX gapY extraCondGapX conditionBranchGapY flow ->
            ( Flow.Layout.layout (Pos x y)
                { nodeProperties = \_ -> { width = w, height = h, marginLeft = ml, marginRight = mr }
                , connectionY = always nodeConnectionY
                , gapX = gapX
                , gapY = gapY
                , conditionBranchGapX = gapX + extraCondGapX
                , conditionBranchGapY = conditionBranchGapY
                , isConditionRoot = \_ -> True
                }
                flow
            , flow
            )
        )
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 1)
        |> Fuzz.andMap (Fuzz.intAtLeast 1)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap flowFuzzer

module Unit.Flow exposing
    ( suite
    , flowFuzzer, flowFuzzerOfMaxDepth, flowWithUniqueIdsFuzzer
    , pathFuzzer
    )

{-|

@docs suite
@docs flowFuzzer, flowFuzzerOfMaxDepth, flowWithUniqueIdsFuzzer
@docs pathFuzzer

-}

import Dict
import Dict.Extra
import Expect exposing (Expectation)
import Flow exposing (Flow(..), Step(..))
import Fuzz exposing (Fuzzer)
import List.Extra
import Set
import Test exposing (Test)
import Test.Distribution


suite : Test
suite =
    Test.describe "Flow"
        [ optimize
        , descendants
        , navigateTo
        , mapWithNode
        , mapAt
        , filter
        , leftEdge
        , rightEdgeNodes
        , dependencies
        , dependants
        , previousPath
        , nextPath
        , edges
        , diff
        , nodes
        , isEmpty
        , toDot
        , breadthFirstFold
        ]


eCfg =
    { nodeToId = id }


id : Int -> String
id n =
    n
        |> String.fromInt


optimize : Test
optimize =
    Test.describe "optimize"
        [ let
            isSequentialWithSequentialChild flow =
                case flow of
                    Sequential xs ->
                        List.any Flow.isSequential xs

                    _ ->
                        False
          in
          Test.fuzz flowFuzzer "After optimize, there are no immediately-nested Sequentials" <|
            \flow ->
                Flow.optimize flow
                    |> Flow.descendants
                    -- We don't need to check `flow` itself because `descendants` do contain it
                    |> List.any isSequentialWithSequentialChild
                    |> Expect.equal False
                    |> Expect.onFail "Found Sequential [..., Sequential[...], ...] - Flow.optimize should have flattened that"
        , let
            isParallelWithParallelChild flow =
                case flow of
                    Parallel xs ->
                        List.any Flow.isParallel xs

                    _ ->
                        False
          in
          Test.fuzz flowFuzzer "After optimize, there are no immediately-nested Parallels" <|
            \flow ->
                Flow.optimize flow
                    |> Flow.descendants
                    -- We don't need to check `flow` itself because `descendants` do contain it
                    |> List.any isParallelWithParallelChild
                    |> Expect.equal False
                    |> Expect.onFail "Found Parallel [..., Parallel[...], ...] - Flow.optimize should have flattened that"
        , let
            isSingletonSequential flow =
                case flow of
                    Sequential [ _ ] ->
                        True

                    _ ->
                        False
          in
          Test.fuzz flowFuzzer "After optimize, there are no singleton Sequentials" <|
            \flow ->
                Flow.optimize flow
                    |> Flow.descendants
                    -- We don't need to check `flow` itself because `descendants` do contain it
                    |> List.any isSingletonSequential
                    |> Expect.equal False
                    |> Expect.onFail "Found Sequential [singleItem] - Flow.optimize should have flattened that"
        , let
            isSingletonParallel flow =
                case flow of
                    Parallel [ _ ] ->
                        True

                    _ ->
                        False
          in
          Test.fuzz flowFuzzer "After optimize, there are no singleton Parallels" <|
            \flow ->
                Flow.optimize flow
                    |> Flow.descendants
                    -- We don't need to check `flow` itself because `descendants` do contain it
                    |> List.any isSingletonParallel
                    |> Expect.equal False
                    |> Expect.onFail "Found Parallel [singleItem] - Flow.optimize should have flattened that"
        , let
            isEmptyParallel flow =
                case flow of
                    Parallel [] ->
                        True

                    _ ->
                        False
          in
          Test.fuzz flowFuzzer "After optimize, there are no empty Parallels" <|
            \flow ->
                Flow.optimize flow
                    |> Flow.descendants
                    -- We don't need to check `flow` itself because `descendants` do contain it
                    |> List.any isEmptyParallel
                    |> Expect.equal False
                    |> Expect.onFail "Found Parallel [] - Flow.optimize should have replaced that with Sequential []"
        , Test.test "Empty Sequential in Parallel gets optimized out" <|
            \() ->
                Flow.optimize (Parallel [ Sequential [], Node 0, Node 1 ])
                    |> Expect.equal (Parallel [ Node 0, Node 1 ])
        ]


descendants : Test
descendants =
    Test.describe "descendants"
        [ Test.test "Example from doc comment" <|
            \() ->
                Sequential
                    [ Node 1
                    , Parallel [ Node 2, Node 3 ]
                    , Condition 4 { trueSeq = [ Node 5 ], falseSeq = [ Node 6 ] }
                    ]
                    |> Flow.descendants
                    |> Expect.equalLists
                        [ Sequential [ Node 1, Parallel [ Node 2, Node 3 ], Condition 4 { trueSeq = [ Node 5 ], falseSeq = [ Node 6 ] } ]
                        , Node 1
                        , Parallel [ Node 2, Node 3 ]
                        , Node 2
                        , Node 3
                        , Condition 4 { trueSeq = [ Node 5 ], falseSeq = [ Node 6 ] }
                        , Node 5
                        , Node 6
                        ]
        ]


navigateTo : Test
navigateTo =
    Test.describe "navigateTo"
        [ Test.fuzz flowFuzzer "empty path" <|
            \flow ->
                Flow.navigateTo [] flow
                    |> Expect.equal (Just flow)
        , Test.describe "InSequential"
            [ Test.test "Node" <|
                \() ->
                    Flow.navigateTo [ InSequential 0 ] (Node 0)
                        |> Expect.equal Nothing
            , Test.test "empty Sequential" <|
                \() ->
                    Flow.navigateTo [ InSequential 0 ] (Sequential [])
                        |> Expect.equal Nothing
            , Test.fuzz (nonemptyShortListFuzzer flowFuzzer |> withIndex) "long enough Sequential" <|
                \( xs, i ) ->
                    Flow.navigateTo [ InSequential i ] (Sequential xs)
                        |> Expect.notEqual Nothing
            , Test.test "long enough Sequential example" <|
                \() ->
                    Flow.navigateTo [ InSequential 1 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> Expect.equal (Just (Node 1))
            , Test.fuzz2 (shortListFuzzer flowFuzzer) (Fuzz.intRange 1 10) "not long enough Sequential" <|
                \xs i ->
                    Flow.navigateTo [ InSequential (List.length xs + i) ] (Sequential xs)
                        |> Expect.equal Nothing
            , Test.fuzz2 (Fuzz.intRange 0 10) (shortListFuzzer flowFuzzer) "Parallel" <|
                \i xs ->
                    Flow.navigateTo [ InSequential i ] (Parallel xs)
                        |> Expect.equal Nothing
            , Test.fuzz3 (Fuzz.intRange 0 10) (shortListFuzzer flowFuzzer) (shortListFuzzer flowFuzzer) "Condition" <|
                \i t f ->
                    Flow.navigateTo [ InSequential i ] (Condition 0 { trueSeq = t, falseSeq = f })
                        |> Expect.equal Nothing
            ]
        , Test.describe "InParallel"
            [ Test.test "Node" <|
                \() ->
                    Flow.navigateTo [ InParallel 0 ] (Node 0)
                        |> Expect.equal Nothing
            , Test.fuzz2 (Fuzz.intRange 0 10) (shortListFuzzer flowFuzzer) "Sequential" <|
                \i xs ->
                    Flow.navigateTo [ InParallel i ] (Sequential xs)
                        |> Expect.equal Nothing
            , Test.test "empty Parallel" <|
                \() ->
                    Flow.navigateTo [ InParallel 0 ] (Parallel [])
                        |> Expect.equal Nothing
            , Test.fuzz (nonemptyShortListFuzzer flowFuzzer |> withIndex) "long enough Parallel" <|
                \( xs, i ) ->
                    Flow.navigateTo [ InParallel i ] (Parallel xs)
                        |> Expect.notEqual Nothing
            , Test.test "long enough Parallel - example" <|
                \() ->
                    Flow.navigateTo [ InParallel 1 ] (Parallel [ Node 0, Node 1, Node 2 ])
                        |> Expect.equal (Just (Node 1))
            , Test.fuzz2 (shortListFuzzer flowFuzzer) (Fuzz.intRange 1 10) "not long enough Parallel" <|
                \xs i ->
                    Flow.navigateTo [ InParallel (List.length xs + i) ] (Parallel xs)
                        |> Expect.equal Nothing
            , Test.fuzz3 (Fuzz.intRange 0 10) (shortListFuzzer flowFuzzer) (shortListFuzzer flowFuzzer) "Condition" <|
                \i t f ->
                    Flow.navigateTo [ InParallel i ] (Condition 0 { trueSeq = t, falseSeq = f })
                        |> Expect.equal Nothing
            ]
        , Test.describe "InConditionTrue"
            [ Test.test "Node" <|
                \() ->
                    Flow.navigateTo [ InConditionTrue 0 ] (Node 0)
                        |> Expect.equal Nothing
            , Test.fuzz2 (Fuzz.intRange 0 10) (shortListFuzzer flowFuzzer) "Sequential" <|
                \i xs ->
                    Flow.navigateTo [ InConditionTrue i ] (Sequential xs)
                        |> Expect.equal Nothing
            , Test.fuzz2 (Fuzz.intRange 0 10) (shortListFuzzer flowFuzzer) "Parallel" <|
                \i xs ->
                    Flow.navigateTo [ InConditionTrue i ] (Parallel xs)
                        |> Expect.equal Nothing
            , Test.test "Condition with empty trueSeq" <|
                \() ->
                    Flow.navigateTo [ InConditionTrue 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> Expect.equal Nothing
            , Test.fuzz2
                (shortListFuzzer flowFuzzer)
                (nonemptyShortListFuzzer flowFuzzer |> withIndex)
                "Condition with long enough trueSeq"
              <|
                \f ( t, i ) ->
                    Flow.navigateTo [ InConditionTrue i ] (Condition 0 { trueSeq = t, falseSeq = f })
                        |> Expect.notEqual Nothing
            , Test.test "Condition with long enough trueSeq - example" <|
                \() ->
                    Flow.navigateTo [ InConditionTrue 1 ] (Condition 0 { trueSeq = [ Node 0, Node 1, Node 2 ], falseSeq = [] })
                        |> Expect.equal (Just (Node 1))
            , Test.fuzz3
                (shortListFuzzer flowFuzzer)
                (shortListFuzzer flowFuzzer)
                (Fuzz.intRange 1 10)
                "Condition with not long enough trueSeq"
              <|
                \f t i ->
                    Flow.navigateTo [ InConditionTrue (List.length t + i) ] (Condition 0 { trueSeq = t, falseSeq = f })
                        |> Expect.equal Nothing
            ]
        , Test.describe "InConditionFalse"
            [ Test.test "Node" <|
                \() ->
                    Flow.navigateTo [ InConditionFalse 0 ] (Node 0)
                        |> Expect.equal Nothing
            , Test.fuzz2 (Fuzz.intRange 0 10) (shortListFuzzer flowFuzzer) "Sequential" <|
                \i xs ->
                    Flow.navigateTo [ InConditionFalse i ] (Sequential xs)
                        |> Expect.equal Nothing
            , Test.fuzz2 (Fuzz.intRange 0 10) (shortListFuzzer flowFuzzer) "Parallel" <|
                \i xs ->
                    Flow.navigateTo [ InConditionFalse i ] (Parallel xs)
                        |> Expect.equal Nothing
            , Test.test "Condition with empty falseSeq" <|
                \() ->
                    Flow.navigateTo [ InConditionFalse 0 ] (Condition 0 { falseSeq = [], trueSeq = [] })
                        |> Expect.equal Nothing
            , Test.fuzz2
                (shortListFuzzer flowFuzzer)
                (nonemptyShortListFuzzer flowFuzzer |> withIndex)
                "Condition with long enough falseSeq"
              <|
                \t ( f, i ) ->
                    Flow.navigateTo [ InConditionFalse i ] (Condition 0 { falseSeq = f, trueSeq = t })
                        |> Expect.notEqual Nothing
            , Test.test "Condition with long enough falseSeq - example" <|
                \() ->
                    Flow.navigateTo [ InConditionFalse 1 ] (Condition 0 { falseSeq = [ Node 0, Node 1, Node 2 ], trueSeq = [] })
                        |> Expect.equal (Just (Node 1))
            , Test.fuzz3
                (shortListFuzzer flowFuzzer)
                (shortListFuzzer flowFuzzer)
                (Fuzz.intRange 1 10)
                "Condition with not long enough falseSeq"
              <|
                \f t i ->
                    Flow.navigateTo [ InConditionFalse (List.length t + i) ] (Condition 0 { falseSeq = t, trueSeq = f })
                        |> Expect.equal Nothing
            ]
        ]


mapRootNode : (node -> node) -> Flow node -> Flow node
mapRootNode fn flow =
    case flow of
        Node n ->
            Node (fn n)

        Condition n r ->
            Condition (fn n) r

        Sequential _ ->
            flow

        Parallel _ ->
            flow


mapWithNode : Test
mapWithNode =
    Test.describe "mapWithNode"
        [ Test.fuzz flowFuzzer "if ignoring first argument, behaves like map" <|
            \flow ->
                Flow.mapWithNode (\_ i -> i + 1) flow
                    |> Expect.equal (Flow.map (\i -> i + 1) flow)
        ]


mapAt : Test
mapAt =
    let
        incre =
            mapRootNode (\x -> x + 1)
    in
    -- TODO there might be more tests to write, now that we've changed mapAt to take `Flow node -> Flow node` instead of `node -> node`
    Test.describe "mapAt"
        [ Test.describe "empty path"
            [ Test.fuzz flowFuzzer "Changes if the flow has a node, leaves as-is otherwise" <|
                \flow ->
                    let
                        expectation =
                            case flow of
                                Node _ ->
                                    Expect.notEqual

                                Condition _ _ ->
                                    Expect.notEqual

                                Sequential _ ->
                                    Expect.equal

                                Parallel _ ->
                                    Expect.equal
                    in
                    Flow.mapAt [] incre flow
                        |> expectation flow
            , Test.test "Node - updates the node - specific example" <|
                \() ->
                    Flow.mapAt [] incre (Node 0)
                        |> Expect.equal (Node 1)
            , Test.test "Condition - updates the node - specific example" <|
                \() ->
                    Flow.mapAt [] incre (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> Expect.equal (Condition 1 { trueSeq = [], falseSeq = [] })
            ]
        , Test.describe "InSequential"
            [ Test.fuzz2 flowFuzzer (Fuzz.intRange 0 10) "Safety: path doesn't match flow -> no change" <|
                \flow i ->
                    let
                        expectation =
                            case flow of
                                Node _ ->
                                    Expect.equal flow

                                Condition _ _ ->
                                    Expect.equal flow

                                Sequential xs ->
                                    if i < List.length xs then
                                        -- It would be tricky to make an `Expect.notEqual` assertion since we could get things like `Seq [Seq []]`
                                        \_ -> Expect.pass

                                    else
                                        Expect.equal flow

                                Parallel _ ->
                                    Expect.equal flow
                    in
                    Flow.mapAt [ InSequential i ] incre flow
                        |> expectation
            , Test.test "Liveness: path matches, node exists -> change" <|
                \() ->
                    Flow.mapAt [ InSequential 1 ] incre (Sequential [ Node 0, Node 1, Node 2 ])
                        |> Expect.equal (Sequential [ Node 0, Node 2, Node 2 ])
            ]
        , Test.describe "InParallel"
            [ Test.fuzz2 flowFuzzer (Fuzz.intRange 0 10) "Safety: path doesn't match flow -> no change" <|
                \flow i ->
                    let
                        expectation =
                            case flow of
                                Node _ ->
                                    Expect.equal flow

                                Condition _ _ ->
                                    Expect.equal flow

                                Parallel xs ->
                                    if i < List.length xs then
                                        -- It would be tricky to make an `Expect.notEqual` assertion since we could get things like `Par [Par []]`
                                        \_ -> Expect.pass

                                    else
                                        Expect.equal flow

                                Sequential _ ->
                                    Expect.equal flow
                    in
                    Flow.mapAt [ InParallel i ] incre flow
                        |> expectation
            , Test.test "Liveness: path matches, node exists -> change" <|
                \() ->
                    Flow.mapAt [ InParallel 1 ] incre (Parallel [ Node 0, Node 1, Node 2 ])
                        |> Expect.equal (Parallel [ Node 0, Node 2, Node 2 ])
            ]
        , Test.describe "InConditionTrue"
            [ Test.fuzz2 flowFuzzer (Fuzz.intRange 0 10) "Safety: path doesn't match flow -> no change" <|
                \flow i ->
                    let
                        expectation =
                            case flow of
                                Node _ ->
                                    Expect.equal flow

                                Condition _ { trueSeq } ->
                                    if i < List.length trueSeq then
                                        -- It would be tricky to make an `Expect.notEqual` assertion
                                        \_ -> Expect.pass

                                    else
                                        Expect.equal flow

                                Parallel _ ->
                                    Expect.equal flow

                                Sequential _ ->
                                    Expect.equal flow
                    in
                    Flow.mapAt [ InConditionTrue i ] incre flow
                        |> expectation
            , Test.test "Liveness: path matches, node exists -> change" <|
                \() ->
                    Flow.mapAt [ InConditionTrue 1 ] incre (Condition 0 { trueSeq = [ Node 1, Node 2, Node 3 ], falseSeq = [] })
                        |> Expect.equal (Condition 0 { trueSeq = [ Node 1, Node 3, Node 3 ], falseSeq = [] })
            ]
        , Test.describe "InConditionFalse"
            [ Test.fuzz2 flowFuzzer (Fuzz.intRange 0 10) "Safety: path doesn't match flow -> no change" <|
                \flow i ->
                    let
                        expectation =
                            case flow of
                                Node _ ->
                                    Expect.equal flow

                                Condition _ { falseSeq } ->
                                    if i < List.length falseSeq then
                                        -- It would be tricky to make an `Expect.notEqual` assertion
                                        \_ -> Expect.pass

                                    else
                                        Expect.equal flow

                                Parallel _ ->
                                    Expect.equal flow

                                Sequential _ ->
                                    Expect.equal flow
                    in
                    Flow.mapAt [ InConditionFalse i ] incre flow
                        |> expectation
            , Test.test "Liveness: path matches, node exists -> change" <|
                \() ->
                    Flow.mapAt [ InConditionFalse 1 ] incre (Condition 0 { falseSeq = [ Node 1, Node 2, Node 3 ], trueSeq = [] })
                        |> Expect.equal (Condition 0 { falseSeq = [ Node 1, Node 3, Node 3 ], trueSeq = [] })
            ]
        , Test.describe "Complex paths"
            [ Test.test "All steps" <|
                \() ->
                    Flow.mapAt
                        [ InSequential 1
                        , InParallel 2
                        , InConditionTrue 3
                        , InConditionFalse 4
                        ]
                        incre
                        (Sequential
                            [ Node 0
                            , Parallel
                                [ Node 1
                                , Node 2
                                , Condition 3
                                    { falseSeq = [ Node 4 ]
                                    , trueSeq =
                                        [ Node 5
                                        , Node 6
                                        , Node 7
                                        , Condition 8
                                            { trueSeq = [ Node 9 ]

                                            --------------------------------------------------------v
                                            , falseSeq = [ Node 10, Node 11, Node 12, Node 13, Node 14, Node 15 ]
                                            }
                                        , Node 16
                                        ]
                                    }
                                , Node 17
                                ]
                            , Node 18
                            ]
                        )
                        |> Expect.equal
                            (Sequential
                                [ Node 0
                                , Parallel
                                    [ Node 1
                                    , Node 2
                                    , Condition 3
                                        { falseSeq = [ Node 4 ]
                                        , trueSeq =
                                            [ Node 5
                                            , Node 6
                                            , Node 7
                                            , Condition 8
                                                { trueSeq = [ Node 9 ]

                                                --------------------------------------------------------v
                                                , falseSeq = [ Node 10, Node 11, Node 12, Node 13, Node 15, Node 15 ]
                                                }
                                            , Node 16
                                            ]
                                        }
                                    , Node 17
                                    ]
                                , Node 18
                                ]
                            )
            ]
        ]


filter : Test
filter =
    Test.describe "Filtering node"
        [ Test.test "Filter nodes" <|
            \() ->
                Sequential
                    [ Node 1
                    , Parallel
                        [ Node 2
                        , Node 3
                        , Condition 4
                            { falseSeq = [ Node 5 ]
                            , trueSeq =
                                [ Node 6
                                , Node 7
                                , Node 8
                                , Condition 9
                                    { trueSeq = [ Node 10 ]
                                    , falseSeq =
                                        [ Node 11
                                        , Node 12
                                        ]
                                    }
                                , Node 13
                                ]
                            }
                        , Node 14
                        ]
                    , Node 15
                    ]
                    |> Flow.filter (modBy 2 >> (==) 0)
                    |> Expect.equal
                        (Sequential
                            [ Sequential []
                            , Parallel
                                [ Node 2
                                , Sequential []
                                , Condition 4
                                    { falseSeq = [ Sequential [] ]
                                    , trueSeq =
                                        [ Node 6
                                        , Sequential []
                                        , Node 8
                                        , Sequential []
                                        , Sequential []
                                        ]
                                    }
                                , Node 14
                                ]
                            , Sequential []
                            ]
                        )
        , Test.fuzz flowFuzzer "Keep all nodes" <|
            \flow ->
                Flow.filter (always True) flow
                    |> Expect.equal flow
        ]


leftEdge : Test
leftEdge =
    Test.describe "leftEdge"
        [ Test.fuzz (Fuzz.intAtLeast 0) "Node" <|
            \i ->
                Flow.leftEdge (Node i)
                    |> Expect.equal [ { node = i, path = [] } ]
        , Test.fuzz3 (Fuzz.intAtLeast 0) (shortListFuzzer flowFuzzer) (shortListFuzzer flowFuzzer) "Condition" <|
            \i t f ->
                Flow.leftEdge (Condition i { trueSeq = t, falseSeq = f })
                    |> Expect.equal [ { node = i, path = [] } ]
        , Test.test "Sequential - empty" <|
            \() ->
                Flow.leftEdge (Sequential [])
                    |> Expect.equal []
        , Test.fuzz2 (Fuzz.intAtLeast 0) (shortListFuzzer flowFuzzer) "Sequential - known init" <|
            \i xs ->
                Flow.leftEdge (Sequential (Node i :: xs))
                    |> Expect.equal [ { node = i, path = [ InSequential 0 ] } ]
        , Test.test "Parallel - empty" <|
            \() ->
                Flow.leftEdge (Parallel [])
                    |> Expect.equal []
        , Test.test "Parallel - non-empty example" <|
            \() ->
                Flow.leftEdge
                    (Parallel
                        [ Node 1
                        , Sequential [ Node 2, Node 3 ]
                        , Condition 4 { trueSeq = [], falseSeq = [] }
                        ]
                    )
                    |> Expect.equal
                        [ { node = 1, path = [ InParallel 0 ] }
                        , { node = 2, path = [ InParallel 1, InSequential 0 ] }
                        , { node = 4, path = [ InParallel 2 ] }
                        ]
        , Test.fuzz (nonemptyShortListFuzzer flowFuzzer) "Parallel - non-empty" <|
            \xs ->
                let
                    expectation =
                        if List.all (List.isEmpty << Flow.leftEdge) xs then
                            Expect.equal []

                        else
                            Expect.notEqual []
                in
                Flow.leftEdge (Parallel xs)
                    |> expectation
        , Test.fuzz emptyFlowFuzzer "Leading empty flow" <|
            \emptyFlow ->
                Flow.leftEdge (Sequential [ emptyFlow, Node 0 ])
                    |> Expect.equal
                        [ { node = 0, path = [ InSequential 1 ] }
                        ]
        ]


rightEdgeNodes : Test
rightEdgeNodes =
    Test.describe "rightEdgeNodes"
        [ Test.test "Node" <|
            \() ->
                Node "A"
                    |> Flow.rightEdgeNodes
                    |> Expect.equal [ "A" ]
        , Test.test "Sequential" <|
            \() ->
                Sequential
                    [ Node "A"
                    , Node "B"
                    ]
                    |> Flow.rightEdgeNodes
                    |> Expect.equal [ "B" ]
        , Test.test "Parallel" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Node "B"
                    ]
                    |> Flow.rightEdgeNodes
                    |> Expect.equal [ "A", "B" ]
        , Test.test "Condition (empty)" <|
            \() ->
                Condition "A"
                    { trueSeq = []
                    , falseSeq = []
                    }
                    |> Flow.rightEdgeNodes
                    |> Expect.equal [ "A" ]
        , Test.test "Condition (true non-empty)" <|
            \() ->
                Condition "A"
                    { trueSeq = [ Node "B" ]
                    , falseSeq = []
                    }
                    |> Flow.rightEdgeNodes
                    |> Expect.equal [ "A", "B" ]
        , Test.test "Condition (false non-empty)" <|
            \() ->
                Condition "A"
                    { trueSeq = []
                    , falseSeq = [ Node "B" ]
                    }
                    |> Flow.rightEdgeNodes
                    |> Expect.equal [ "A", "B" ]
        , Test.test "Condition (both non-empty)" <|
            \() ->
                Condition "A"
                    { trueSeq = [ Node "B" ]
                    , falseSeq = [ Node "C" ]
                    }
                    |> Flow.rightEdgeNodes
                    |> Expect.equal [ "B", "C" ]
        , Test.test "Complex example" <|
            \() ->
                Parallel
                    [ Node "A"
                    , Sequential [ Node "B", Node "C" ]
                    , Parallel
                        [ Node "D"
                        , Condition "E" { trueSeq = [], falseSeq = [] }
                        , Sequential
                            [ Node "F"
                            , Condition "G"
                                { trueSeq = [ Node "H" ]
                                , falseSeq = [ Node "I" ]
                                }
                            ]
                        ]
                    ]
                    |> Flow.rightEdgeNodes
                    |> Expect.equal [ "A", "C", "D", "E", "H", "I" ]
        , Test.fuzz flowFuzzer "invariant under `optimize`" <|
            \flow ->
                Flow.rightEdgeNodes flow
                    |> Expect.equal (Flow.rightEdgeNodes (Flow.optimize flow))
        ]


dependencies : Test
dependencies =
    Test.describe "dependencies"
        [ Test.fuzz flowFuzzer "Empty path -> no deps" <|
            \flow ->
                Flow.dependencies [] flow
                    |> Expect.equal []
        , Test.fuzz2 flowFuzzer pathFuzzer "First item of sequential has same deps as the sequential itself" <|
            \flow path ->
                let
                    flowWithSequentialLeaf =
                        Flow.mapAt path (\f -> Sequential [ f ]) flow
                in
                Flow.dependencies (path ++ [ InSequential 0 ]) flowWithSequentialLeaf
                    |> Expect.equal (Flow.dependencies path flowWithSequentialLeaf)
        , Test.test "Sequential example" <|
            \() ->
                Flow.dependencies [ InSequential 1 ] (Sequential [ Node 0, Node 1 ])
                    |> Expect.equal [ { node = 0, branch = 0 } ]
        , Test.test "Parallel in Sequential example" <|
            \() ->
                Flow.dependencies [ InSequential 1 ] (Sequential [ Parallel [ Node 0, Node 1 ], Node 2 ])
                    |> Expect.equal [ { node = 0, branch = 0 }, { node = 1, branch = 0 } ]
        , Test.test "Nesting example" <|
            \() ->
                Flow.dependencies [ InSequential 1 ]
                    (Sequential
                        [ Parallel
                            [ Sequential []
                            , Sequential [ Node 0 ]
                            , Node 1
                            ]
                        , Node 2
                        ]
                    )
                    |> Expect.equal [ { node = 0, branch = 0 }, { node = 1, branch = 0 } ]
        , Test.test "Long sequential example" <|
            \() ->
                Flow.dependencies [ InSequential 2 ] (Sequential [ Node 0, Node 1, Node 2 ])
                    |> Expect.equal [ { node = 1, branch = 0 } ]
        , Test.fuzz
            (nonemptyShortListFuzzer flowFuzzer
                |> Fuzz.andThen
                    (\rows ->
                        Fuzz.intRange 0 (List.length rows - 1)
                            |> Fuzz.map (\i -> ( rows, i ))
                    )
            )
            "Parallel rows depend on the item preceding the Parallel"
          <|
            \( parallelRows, i ) ->
                case List.Extra.getAt i parallelRows of
                    Nothing ->
                        Expect.fail "Wrong fuzzed index"

                    Just row ->
                        if Flow.leftEdge row == [] then
                            Expect.pass

                        else
                            Flow.dependencies [ InSequential 1, InParallel i ] (Sequential [ Node 0, Parallel parallelRows ])
                                |> Expect.equal [ { node = 0, branch = 0 } ]
        , Test.test "Condition depends on the item preceding it" <|
            \() ->
                Flow.dependencies [ InSequential 1 ] (Sequential [ Node 0, Condition 1 { trueSeq = [], falseSeq = [] } ])
                    |> Expect.equal [ { node = 0, branch = 0 } ]
        , Test.test "Condition trueSeq 1st child depends on the Condition with branch=0" <|
            \() ->
                Flow.dependencies [ InConditionTrue 0 ] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [] })
                    |> Expect.equal [ { node = 0, branch = 0 } ]
        , Test.test "Condition falseSeq 1st child depends on the Condition with branch=1" <|
            \() ->
                Flow.dependencies [ InConditionFalse 0 ] (Condition 0 { falseSeq = [ Node 1 ], trueSeq = [] })
                    |> Expect.equal [ { node = 0, branch = 1 } ]
        , Test.test "Condition trueSeq 2nd+ child depends on item preceding it" <|
            \() ->
                Flow.dependencies [ InConditionTrue 1 ] (Condition 0 { trueSeq = [ Node 1, Node 2 ], falseSeq = [] })
                    |> Expect.equal [ { node = 1, branch = 0 } ]
        , Test.test "Condition falseSeq 2nd+ child depends on item preceding it" <|
            \() ->
                Flow.dependencies [ InConditionFalse 1 ] (Condition 0 { falseSeq = [ Node 1, Node 2 ], trueSeq = [] })
                    |> Expect.equal [ { node = 1, branch = 0 } ]
        ]


dependants : Test
dependants =
    Test.describe "dependants"
        [ Test.fuzz flowFuzzer "Single non-condition item -> no dependants" <|
            \flow ->
                if Flow.isCondition flow then
                    Expect.pass

                else
                    Flow.dependants [] flow
                        |> Expect.equal []
        , Test.fuzz (nonemptyShortListFuzzer flowFuzzer) "Last non-condition item in a top-level sequential -> no dependants" <|
            \xs ->
                let
                    lastI =
                        List.length xs - 1
                in
                case List.Extra.getAt lastI xs of
                    Nothing ->
                        Expect.fail "Buggy fuzzer"

                    Just (Condition _ _) ->
                        Expect.pass

                    Just _ ->
                        Flow.dependants [ InSequential lastI ] (Sequential xs)
                            |> Expect.equal []
        , Test.fuzz (nonemptyShortListFuzzer flowFuzzer |> withIndex) "Any non-condition item in a top-level parallel -> no dependants" <|
            \( xs, i ) ->
                case List.Extra.getAt i xs of
                    Nothing ->
                        Expect.fail "Buggy fuzzer"

                    Just (Condition _ _) ->
                        Expect.pass

                    Just _ ->
                        Flow.dependants [ InParallel i ] (Parallel xs)
                            |> Expect.equal []
        , Test.test "[>0,1]" <|
            \() ->
                Flow.dependants [ InSequential 0 ] (Sequential [ Node 0, Node 1 ])
                    |> Expect.equal [ { path = [ InSequential 1 ], node = 1, branch = 0 } ]
        , Test.test "[0,>1,2]" <|
            \() ->
                Flow.dependants [ InSequential 1 ] (Sequential [ Node 0, Node 1, Node 2 ])
                    |> Expect.equal [ { path = [ InSequential 2 ], node = 2, branch = 0 } ]
        , Test.test "[>0,{1,2}]" <|
            \() ->
                Flow.dependants [ InSequential 0 ] (Sequential [ Node 0, Parallel [ Node 1, Node 2 ] ])
                    |> Expect.equal
                        [ { path = [ InSequential 1, InParallel 0 ], node = 1, branch = 0 }
                        , { path = [ InSequential 1, InParallel 1 ], node = 2, branch = 0 }
                        ]
        , Test.test ">0?([1],[2])" <|
            \() ->
                Flow.dependants [] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                    |> Expect.equal
                        [ { path = [ InConditionTrue 0 ], node = 1, branch = 0 }
                        , { path = [ InConditionFalse 0 ], node = 2, branch = 1 }
                        ]
        , Test.test ">0?([1,2],[3,4])" <|
            \() ->
                Flow.dependants [] (Condition 0 { trueSeq = [ Node 1, Node 2 ], falseSeq = [ Node 3, Node 4 ] })
                    |> Expect.equal
                        [ { path = [ InConditionTrue 0 ], node = 1, branch = 0 }
                        , { path = [ InConditionFalse 0 ], node = 3, branch = 1 }
                        ]
        , Test.test "[>0?([],[]),1]" <|
            \() ->
                Flow.dependants [ InSequential 0 ] (Sequential [ Condition 0 { trueSeq = [], falseSeq = [] }, Node 1 ])
                    |> Expect.equal
                        [ { path = [ InSequential 1 ], node = 1, branch = 0 }
                        , { path = [ InSequential 1 ], node = 1, branch = 1 }
                        ]
        , Test.test "[0?([>1],[2]),3]" <|
            \() ->
                Flow.dependants [ InSequential 0, InConditionTrue 0 ]
                    (Sequential
                        [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] }
                        , Node 3
                        ]
                    )
                    |> Expect.equal [ { path = [ InSequential 1 ], node = 3, branch = 0 } ]
        , Test.test "[0?([1],[>2]),3]" <|
            \() ->
                Flow.dependants [ InSequential 0, InConditionFalse 0 ]
                    (Sequential
                        [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] }
                        , Node 3
                        ]
                    )
                    |> Expect.equal [ { path = [ InSequential 1 ], node = 3, branch = 0 } ]
        , Test.test "[>0?([1],[]),2]" <|
            \() ->
                Flow.dependants [ InSequential 0 ]
                    (Sequential
                        [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [] }
                        , Node 2
                        ]
                    )
                    |> Expect.equal
                        [ { path = [ InSequential 0, InConditionTrue 0 ], node = 1, branch = 0 }
                        , { path = [ InSequential 1 ], node = 2, branch = 1 }
                        ]
        , Test.test "[>0?([],[1]),2]" <|
            \() ->
                Flow.dependants [ InSequential 0 ]
                    (Sequential
                        [ Condition 0 { trueSeq = [], falseSeq = [ Node 1 ] }
                        , Node 2
                        ]
                    )
                    |> Expect.equal
                        [ { path = [ InSequential 1 ], node = 2, branch = 0 }
                        , { path = [ InSequential 0, InConditionFalse 0 ], node = 1, branch = 1 }
                        ]
        ]


previousPath : Test
previousPath =
    Test.describe "previousPath"
        [ Test.test "Empty path -> Nothing" <|
            \() ->
                Flow.previousPath []
                    |> Expect.equal Nothing
        , Test.fuzz2 pathFuzzer stepConstructorFuzzer "First item in any step -> move _to_ the step" <|
            \path inStep ->
                Flow.previousPath (path ++ [ inStep 0 ])
                    |> Expect.equal (Just path)
        , Test.fuzz3 pathFuzzer stepConstructorFuzzer (Fuzz.intAtLeast 1) "2nd+ item in any step -> move to the previous item" <|
            \path inStep i ->
                Flow.previousPath (path ++ [ inStep i ])
                    |> Expect.equal (Just (path ++ [ inStep (i - 1) ]))
        ]


nextPath : Test
nextPath =
    Test.describe "nextPath"
        [ Test.fuzz flowFuzzer "Empty path -> Nothing" <|
            \flow ->
                Flow.nextPath [] flow
                    |> Expect.equal Nothing
        , let
            isSequentialWithMoreThanOneItem f =
                case f of
                    Sequential (_ :: _ :: _) ->
                        True

                    _ ->
                        False
          in
          Test.fuzzWith
            { runs = 100
            , distribution =
                Test.expectDistribution
                    [ ( Test.Distribution.atLeast 25, "non-skipped", Flow.descendants >> List.any isSequentialWithMoreThanOneItem )
                    ]
            }
            flowFuzzer
            "Sequential non-last item: move to the next item"
          <|
            \flow ->
                case Flow.find isSequentialWithMoreThanOneItem flow of
                    Nothing ->
                        Expect.pass

                    Just ( _, path ) ->
                        Flow.nextPath (path ++ [ InSequential 0 ]) flow
                            |> Expect.equal (Just (path ++ [ InSequential 1 ]))
        , Test.test "Sequential last item -> move to the sibling of the sequential" <|
            \() ->
                Flow.nextPath [ InSequential 0, InSequential 0 ] (Sequential [ Sequential [ Node 0 ], Node 1 ])
                    |> Expect.equal (Just [ InSequential 1 ])
        , Test.test "Parallel any item -> move to the sibling of the parallel" <|
            \() ->
                Flow.nextPath [ InSequential 0, InParallel 0 ] (Sequential [ Parallel [ Node 0 ], Node 1 ])
                    |> Expect.equal (Just [ InSequential 1 ])

        -- The four tests below are just a copy-paste of the two Sequential tests above.
        , let
            isConditionTrueWithMoreThanOneItem f =
                case f of
                    Condition _ { trueSeq } ->
                        List.length trueSeq > 1

                    _ ->
                        False
          in
          Test.fuzzWith
            { runs = 100
            , distribution =
                Test.expectDistribution
                    [ ( Test.Distribution.atLeast 25, "non-skipped", Flow.descendants >> List.any isConditionTrueWithMoreThanOneItem )
                    ]
            }
            flowFuzzer
            "Condition-True non-last item: move to the next item"
          <|
            \flow ->
                case Flow.find isConditionTrueWithMoreThanOneItem flow of
                    Nothing ->
                        Expect.pass

                    Just ( _, path ) ->
                        Flow.nextPath (path ++ [ InConditionTrue 0 ]) flow
                            |> Expect.equal (Just (path ++ [ InConditionTrue 1 ]))
        , Test.test "Condition-True last item -> move to the sibling of the condition" <|
            \() ->
                Flow.nextPath [ InSequential 0, InConditionTrue 0 ] (Sequential [ Condition 0 { trueSeq = [ Node 0 ], falseSeq = [] }, Node 1 ])
                    |> Expect.equal (Just [ InSequential 1 ])
        , let
            isConditionFalseWithMoreThanOneItem f =
                case f of
                    Condition _ { falseSeq } ->
                        List.length falseSeq > 1

                    _ ->
                        False
          in
          Test.fuzzWith
            { runs = 100
            , distribution =
                Test.expectDistribution
                    [ ( Test.Distribution.atLeast 25, "non-skipped", Flow.descendants >> List.any isConditionFalseWithMoreThanOneItem )
                    ]
            }
            flowFuzzer
            "Condition-False non-last item: move to the next item"
          <|
            \flow ->
                case Flow.find isConditionFalseWithMoreThanOneItem flow of
                    Nothing ->
                        Expect.pass

                    Just ( _, path ) ->
                        Flow.nextPath (path ++ [ InConditionFalse 0 ]) flow
                            |> Expect.equal (Just (path ++ [ InConditionFalse 1 ]))
        , Test.test "Condition-False last item -> move to the sibling of the condition" <|
            \() ->
                Flow.nextPath [ InSequential 0, InConditionFalse 0 ] (Sequential [ Condition 0 { trueSeq = [], falseSeq = [ Node 0 ] }, Node 1 ])
                    |> Expect.equal (Just [ InSequential 1 ])
        ]


expectEqualEdges :
    List { before : String, after : String, branch : Int }
    -> List { before : String, after : String, branch : Int }
    -> Expectation
expectEqualEdges expected actual =
    let
        fn list =
            Set.fromList (List.map (\e -> ( e.before, e.after, e.branch )) list)
    in
    fn actual
        |> Expect.equalSets (fn expected)


edges : Test
edges =
    Test.describe "edges"
        [ Test.test "Node" <|
            \() ->
                Flow.edges eCfg (Node 0)
                    |> expectEqualEdges []
        , Test.test "Sequential - length 0" <|
            \() ->
                Flow.edges eCfg (Sequential [])
                    |> expectEqualEdges []
        , Test.test "Sequential - length 1" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0 ])
                    |> expectEqualEdges []
        , Test.test "Sequential - length 2" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Node 1 ])
                    |> expectEqualEdges [ { before = id 0, after = id 1, branch = 0 } ]
        , Test.test "Sequential - length 3" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Node 1, Node 2 ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 1, after = id 2, branch = 0 }
                        ]
        , Test.test "Node then Parallel" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Parallel [ Node 1, Node 2 ] ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 0, after = id 2, branch = 0 }
                        ]
        , Test.test "Parallel then Node" <|
            \() ->
                Flow.edges eCfg (Sequential [ Parallel [ Node 0, Node 1 ], Node 2 ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 2, branch = 0 }
                        , { before = id 1, after = id 2, branch = 0 }
                        ]
        , Test.test "Empty Sequential between nodes" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Sequential [], Node 1 ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        ]
        , Test.test "Empty Parallel between nodes" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Parallel [], Node 1 ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        ]
        , Test.fuzz emptyFlowFuzzer "Empty flow between nodes" <|
            \emptyFlow ->
                Flow.edges eCfg (Sequential [ Node 0, emptyFlow, Node 1 ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        ]
        , Test.fuzz emptyFlowFuzzer "Leading empty flow" <|
            \emptyFlow ->
                Flow.edges eCfg (Sequential [ emptyFlow, Node 0, Node 1 ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        ]
        , Test.test "Empty condition then Node" <|
            \() ->
                Flow.edges eCfg (Sequential [ Condition 0 { trueSeq = [], falseSeq = [] }, Node 1 ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 0, after = id 1, branch = 1 }
                        ]
        , Test.test "Condition with empty False then Node" <|
            \() ->
                Flow.edges eCfg (Sequential [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [] }, Node 2 ])
                    |> expectEqualEdges
                        [ { before = id 1, after = id 2, branch = 0 }
                        , { before = id 0, after = id 2, branch = 1 }
                        , { before = id 0, after = id 1, branch = 0 }
                        ]
        , Test.test "Condition with empty True then Node" <|
            \() ->
                Flow.edges eCfg (Sequential [ Condition 0 { trueSeq = [], falseSeq = [ Node 1 ] }, Node 2 ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 2, branch = 0 }
                        , { before = id 1, after = id 2, branch = 0 }
                        , { before = id 0, after = id 1, branch = 1 }
                        ]
        , Test.test "Non-empty condition then Node" <|
            \() ->
                Flow.edges eCfg (Sequential [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] }, Node 3 ])
                    |> expectEqualEdges
                        [ { before = id 1, after = id 3, branch = 0 }
                        , { before = id 2, after = id 3, branch = 0 }
                        , { before = id 0, after = id 1, branch = 0 }
                        , { before = id 0, after = id 2, branch = 1 }
                        ]
        , Test.test "Node then empty condition" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Condition 1 { trueSeq = [], falseSeq = [] } ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        ]
        , Test.test "Node then condition with empty False" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Condition 1 { trueSeq = [ Node 2 ], falseSeq = [] } ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 1, after = id 2, branch = 0 }
                        ]
        , Test.test "Node then condition with empty True" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Condition 1 { trueSeq = [], falseSeq = [ Node 2 ] } ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 1, after = id 2, branch = 1 }
                        ]
        , Test.test "Node then non-empty condition" <|
            \() ->
                Flow.edges eCfg (Sequential [ Node 0, Condition 1 { trueSeq = [ Node 2 ], falseSeq = [ Node 3 ] } ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 1, after = id 2, branch = 0 }
                        , { before = id 1, after = id 3, branch = 1 }
                        ]
        , Test.test "Two parallels next to each other" <|
            \() ->
                Flow.edges eCfg (Sequential [ Parallel [ Node 0, Node 1 ], Parallel [ Node 2, Node 3 ] ])
                    |> expectEqualEdges
                        [ { before = id 0, after = id 2, branch = 0 }
                        , { before = id 1, after = id 2, branch = 0 }
                        , { before = id 0, after = id 3, branch = 0 }
                        , { before = id 1, after = id 3, branch = 0 }
                        ]
        , Test.test "Parallel conditions merging" <|
            \() ->
                Flow.edges eCfg
                    (Sequential
                        [ Parallel
                            [ Condition 1
                                { trueSeq = [ Node 2 ]
                                , falseSeq = [ Node 3 ]
                                }
                            , Condition 4
                                { trueSeq = [ Node 5 ]
                                , falseSeq = [ Node 6 ]
                                }
                            ]
                        , Node 7
                        ]
                    )
                    |> expectEqualEdges
                        [ { before = id 1, after = id 2, branch = 0 }
                        , { before = id 1, after = id 3, branch = 1 }
                        , { before = id 4, after = id 5, branch = 0 }
                        , { before = id 4, after = id 6, branch = 1 }
                        , { before = id 2, after = id 7, branch = 0 }
                        , { before = id 3, after = id 7, branch = 0 }
                        , { before = id 5, after = id 7, branch = 0 }
                        , { before = id 6, after = id 7, branch = 0 }
                        ]
        , Test.test "Parallel conditions with empty false branch merging" <|
            \() ->
                Flow.edges eCfg
                    (Sequential
                        [ Parallel
                            [ Condition 1
                                { trueSeq = [ Node 2 ]
                                , falseSeq = []
                                }
                            , Condition 3
                                { trueSeq = [ Node 4 ]
                                , falseSeq = []
                                }
                            ]
                        , Node 5
                        ]
                    )
                    |> expectEqualEdges
                        [ { before = id 1, after = id 2, branch = 0 }
                        , { before = id 2, after = id 5, branch = 0 }
                        , { before = id 1, after = id 5, branch = 1 }
                        , { before = id 3, after = id 4, branch = 0 }
                        , { before = id 4, after = id 5, branch = 0 }
                        , { before = id 3, after = id 5, branch = 1 }
                        ]
        , Test.fuzz emptyFlowFuzzer "Empty flow in condition true" <|
            \emptyFlow ->
                Flow.edges eCfg
                    (Sequential
                        [ Condition 0
                            { trueSeq = [ emptyFlow ]
                            , falseSeq = [ Node 2 ]
                            }
                        , Node 3
                        ]
                    )
                    |> expectEqualEdges
                        [ { before = id 0, after = id 3, branch = 0 }
                        , { before = id 0, after = id 2, branch = 1 }
                        , { before = id 2, after = id 3, branch = 0 }
                        ]
        , Test.fuzz emptyFlowFuzzer "Empty flow in condition false" <|
            \emptyFlow ->
                Flow.edges eCfg
                    (Sequential
                        [ Condition 0
                            { trueSeq = [ Node 1 ]
                            , falseSeq = [ emptyFlow ]
                            }
                        , Node 3
                        ]
                    )
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 1, after = id 3, branch = 0 }
                        , { before = id 0, after = id 3, branch = 1 }
                        ]
        , Test.fuzz emptyFlowFuzzer "Empty flow then node in condition true" <|
            \emptyFlow ->
                Flow.edges eCfg
                    (Sequential
                        [ Condition 0
                            { trueSeq = [ emptyFlow, Node 1 ]
                            , falseSeq = [ Node 2 ]
                            }
                        , Node 3
                        ]
                    )
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 0, after = id 2, branch = 1 }
                        , { before = id 1, after = id 3, branch = 0 }
                        , { before = id 2, after = id 3, branch = 0 }
                        ]
        , Test.fuzz emptyFlowFuzzer "Node then empty flow in condition true" <|
            \emptyFlow ->
                Flow.edges eCfg
                    (Sequential
                        [ Condition 0
                            { trueSeq = [ Node 1, emptyFlow ]
                            , falseSeq = [ Node 2 ]
                            }
                        , Node 3
                        ]
                    )
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 0, after = id 2, branch = 1 }
                        , { before = id 1, after = id 3, branch = 0 }
                        , { before = id 2, after = id 3, branch = 0 }
                        ]
        , Test.fuzz emptyFlowFuzzer "Empty flow then node in condition false" <|
            \emptyFlow ->
                Flow.edges eCfg
                    (Sequential
                        [ Condition 0
                            { trueSeq = [ Node 1 ]
                            , falseSeq = [ emptyFlow, Node 2 ]
                            }
                        , Node 3
                        ]
                    )
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 0, after = id 2, branch = 1 }
                        , { before = id 1, after = id 3, branch = 0 }
                        , { before = id 2, after = id 3, branch = 0 }
                        ]
        , Test.fuzz emptyFlowFuzzer "Node then empty flow in condition false" <|
            \emptyFlow ->
                Flow.edges eCfg
                    (Sequential
                        [ Condition 0
                            { trueSeq = [ Node 1 ]
                            , falseSeq = [ Node 2, emptyFlow ]
                            }
                        , Node 3
                        ]
                    )
                    |> expectEqualEdges
                        [ { before = id 0, after = id 1, branch = 0 }
                        , { before = id 0, after = id 2, branch = 1 }
                        , { before = id 1, after = id 3, branch = 0 }
                        , { before = id 2, after = id 3, branch = 0 }
                        ]
        , Test.fuzz flowWithUniqueIdsFuzzer "Doesn't return duplicate edges" noDuplicateEdges
        , Test.test "Doesn't return duplicate edges - found failure 1" <|
            \() ->
                noDuplicateEdges
                    (Sequential
                        [ Parallel [ Node 1, Node 3 ]
                        , Parallel
                            [ Sequential [ Node 0, Node 4 ]
                            , Node 2
                            ]
                        ]
                    )
        , Test.test "Doesn't return duplicate edges - found failure 2" <|
            \() ->
                noDuplicateEdges
                    (Condition 1
                        { falseSeq =
                            [ Sequential
                                [ Condition 2
                                    { falseSeq = []
                                    , trueSeq = [ Node 3 ]
                                    }
                                ]
                            , Node 0
                            , Node 4
                            ]
                        , trueSeq = []
                        }
                    )
        ]


noDuplicateEdges : Flow Int -> Expectation
noDuplicateEdges flow =
    Flow.edges eCfg flow
        |> Dict.Extra.groupBy
            (\e ->
                ( e.before
                , e.after
                , e.branch
                )
            )
        |> Dict.values
        |> List.filter (\occurrences -> List.length occurrences > 1)
        |> Expect.equalLists []


diff : Test
diff =
    Test.describe "diff"
        [ Test.fuzz flowFuzzer "Equal -> no diff" <|
            \flow ->
                Flow.diff { before = flow, after = flow, nodeToId = id }
                    |> Expect.equal []
        , Test.fuzz2 flowFuzzer flowFuzzer "Symmetry: swap arguments and newDeps+removeDeps will swap as well" <|
            \f1 f2 ->
                -- f1-f2 with swapped new,remove == f2-f1
                Flow.diff { before = f1, after = f2, nodeToId = id }
                    |> List.map (\t -> { t | newDeps = t.removeDeps, removeDeps = t.newDeps })
                    |> Expect.equal (Flow.diff { before = f2, after = f1, nodeToId = id })
        , Test.test "Add an edge" <|
            \() ->
                Flow.diff
                    { before = Node 0
                    , after = Sequential [ Node 0, Node 1 ]
                    , nodeToId = id
                    }
                    |> Expect.equal
                        [ { task = id 1
                          , newDeps = [ { branch = 0, id = id 0 } ]
                          , removeDeps = []
                          }
                        ]
        , Test.test "Remove an edge" <|
            \() ->
                Flow.diff
                    { before = Sequential [ Node 0, Node 1 ]
                    , after = Node 0
                    , nodeToId = id
                    }
                    |> Expect.equal
                        [ { task = id 1
                          , newDeps = []
                          , removeDeps = [ { branch = 0, id = id 0 } ]
                          }
                        ]
        , Test.test "Remove between" <|
            \() ->
                Flow.diff
                    { before = Sequential [ Node 0, Node 1, Node 2 ]
                    , after = Sequential [ Node 0, Node 2 ]
                    , nodeToId = id
                    }
                    |> Expect.equal
                        [ { task = id 1
                          , newDeps = []
                          , removeDeps = [ { branch = 0, id = id 0 } ]
                          }
                        , { task = id 2
                          , newDeps = [ { branch = 0, id = id 0 } ]
                          , removeDeps = [ { branch = 0, id = id 1 } ]
                          }
                        ]
        , Test.fuzz emptyFlowFuzzer "Remove between (unoptimized)" <|
            \emptyFlow ->
                Flow.diff
                    { before = Sequential [ Node 0, Node 1, Node 2 ]
                    , after = Sequential [ Node 0, emptyFlow, Node 2 ]
                    , nodeToId = id
                    }
                    |> Expect.equal
                        [ { task = id 1
                          , newDeps = []
                          , removeDeps = [ { branch = 0, id = id 0 } ]
                          }
                        , { task = id 2
                          , newDeps = [ { branch = 0, id = id 0 } ]
                          , removeDeps = [ { branch = 0, id = id 1 } ]
                          }
                        ]
        , Test.test "Add and remove in different" <|
            \() ->
                Flow.diff
                    { before = Sequential [ Node 0, Node 1 ]
                    , after = Sequential [ Node 0, Parallel [ Node 2, Node 3 ] ]
                    , nodeToId = id
                    }
                    |> Expect.equal
                        [ { task = id 1
                          , newDeps = []
                          , removeDeps = [ { branch = 0, id = id 0 } ]
                          }
                        , { task = id 2
                          , newDeps = [ { branch = 0, id = id 0 } ]
                          , removeDeps = []
                          }
                        , { task = id 3
                          , newDeps = [ { branch = 0, id = id 0 } ]
                          , removeDeps = []
                          }
                        ]
        , Test.test "Add and remove in single" <|
            \() ->
                Flow.diff
                    { before = Sequential [ Node 0, Node 1 ]
                    , after = Sequential [ Node 2, Node 1 ]
                    , nodeToId = id
                    }
                    |> Expect.equal
                        [ { task = id 1
                          , newDeps = [ { branch = 0, id = id 2 } ]
                          , removeDeps = [ { branch = 0, id = id 0 } ]
                          }
                        ]
        ]


nodes : Test
nodes =
    Test.describe "nodes"
        [ Test.test "Node" <|
            \() ->
                Flow.nodes (Node 0)
                    |> Expect.equal [ ( 0, [] ) ]
        , Test.test "Sequential without nodes" <|
            \() ->
                Flow.nodes (Sequential [ Sequential [], Sequential [ Sequential [] ] ])
                    |> Expect.equal []
        , Test.test "Sequential with nodes" <|
            \() ->
                Flow.nodes (Sequential [ Node 0, Sequential [ Node 1, Node 2 ], Sequential [ Sequential [ Node 3 ] ] ])
                    |> Expect.equal
                        [ ( 0, [ InSequential 0 ] )
                        , ( 1, [ InSequential 1, InSequential 0 ] )
                        , ( 2, [ InSequential 1, InSequential 1 ] )
                        , ( 3, [ InSequential 2, InSequential 0, InSequential 0 ] )
                        ]
        , Test.test "Simple sequential" <|
            \() ->
                Flow.nodes (Sequential [ Node 0, Node 1 ])
                    |> Expect.equal
                        [ ( 0, [ InSequential 0 ] )
                        , ( 1, [ InSequential 1 ] )
                        ]
        , Test.test "Parallel without nodes" <|
            \() ->
                Flow.nodes (Parallel [ Parallel [], Parallel [ Parallel [] ] ])
                    |> Expect.equal []
        , Test.test "Parallel with nodes" <|
            \() ->
                Flow.nodes (Parallel [ Node 0, Parallel [ Node 1, Node 2 ], Parallel [ Parallel [ Node 3 ] ] ])
                    |> Expect.equal
                        [ ( 0, [ InParallel 0 ] )
                        , ( 1, [ InParallel 1, InParallel 0 ] )
                        , ( 2, [ InParallel 1, InParallel 1 ] )
                        , ( 3, [ InParallel 2, InParallel 0, InParallel 0 ] )
                        ]
        , Test.test "Empty Condition" <|
            \() ->
                Flow.nodes (Condition 0 { trueSeq = [], falseSeq = [] })
                    |> Expect.equal [ ( 0, [] ) ]
        , Test.test "Condition with trueSeq" <|
            \() ->
                Flow.nodes (Condition 0 { trueSeq = [ Node 1, Node 2 ], falseSeq = [] })
                    |> Expect.equal
                        [ ( 0, [] )
                        , ( 1, [ InConditionTrue 0 ] )
                        , ( 2, [ InConditionTrue 1 ] )
                        ]
        , Test.test "Condition with falseSeq" <|
            \() ->
                Flow.nodes (Condition 0 { trueSeq = [], falseSeq = [ Node 1, Node 2 ] })
                    |> Expect.equal
                        [ ( 0, [] )
                        , ( 1, [ InConditionFalse 0 ] )
                        , ( 2, [ InConditionFalse 1 ] )
                        ]
        , Test.test "Full Condition" <|
            \() ->
                Flow.nodes (Condition 0 { trueSeq = [ Node 1, Node 2 ], falseSeq = [ Node 3, Node 4 ] })
                    |> Expect.equal
                        [ ( 0, [] )
                        , ( 1, [ InConditionTrue 0 ] )
                        , ( 2, [ InConditionTrue 1 ] )
                        , ( 3, [ InConditionFalse 0 ] )
                        , ( 4, [ InConditionFalse 1 ] )
                        ]
        , Test.fuzz flowFuzzer "Same as descendants >> getNode" <|
            \flow ->
                Flow.nodes flow
                    |> List.map Tuple.first
                    |> Set.fromList
                    |> Expect.equalSets
                        (flow
                            |> Flow.descendants
                            |> List.filterMap Flow.getNode
                            |> Set.fromList
                        )
        , Test.fuzz flowFuzzer "nodes themselves invariant under `optimize`" <|
            \flow ->
                List.map Tuple.first (Flow.nodes flow)
                    |> Expect.equal (List.map Tuple.first (Flow.nodes (Flow.optimize flow)))
        ]


isEmpty : Test
isEmpty =
    Test.describe "isEmpty"
        [ Test.test "Node" <|
            \() ->
                Node 0
                    |> Flow.isEmpty
                    |> Expect.equal False
        , Test.test "Condition" <|
            \() ->
                Condition 0 { trueSeq = [], falseSeq = [] }
                    |> Flow.isEmpty
                    |> Expect.equal False
        , Test.test "Sequential with Node" <|
            \() ->
                Sequential [ Node 0 ]
                    |> Flow.isEmpty
                    |> Expect.equal False
        , Test.test "Parallel with Node" <|
            \() ->
                Parallel [ Node 0 ]
                    |> Flow.isEmpty
                    |> Expect.equal False
        , Test.test "Nested" <|
            \() ->
                Sequential [ Sequential [ Parallel [ Node 0 ] ] ]
                    |> Flow.isEmpty
                    |> Expect.equal False
        , Test.fuzz flowFuzzer "isEmpty flow == List.isEmpty (nodes flow)" <|
            \flow ->
                flow
                    |> Flow.isEmpty
                    |> Expect.equal (List.isEmpty (Flow.nodes flow))
        , Test.fuzz emptyFlowFuzzer "Empty flows" <|
            \emptyFlow ->
                emptyFlow
                    |> Flow.isEmpty
                    |> Expect.equal True
        , Test.fuzz flowFuzzer "invariant under `optimize`" <|
            \flow ->
                Flow.isEmpty flow
                    |> Expect.equal (Flow.isEmpty (Flow.optimize flow))
        ]


toDot : Test
toDot =
    Test.describe "toDot"
        [ Test.test "Regression: flow DOT graph containing a condition should have a pink vertex" <|
            -- Related to: https://vendrco.slack.com/archives/C05AYV7D8UC/p1693995546747129?thread_ts=1693995404.172079&cid=C05AYV7D8UC
            -- Related to: https://app.shortcut.com/blissfully/story/90850/flow-parser-dropped-node
            \() ->
                let
                    flow =
                        Sequential
                            [ Node "5"
                            , Parallel
                                [ Condition "2"
                                    { falseSeq = []
                                    , trueSeq = [ Node "3" ]
                                    }
                                , Node "4"
                                ]
                            , Node "0"
                            ]
                in
                flow
                    |> Flow.toDot { nodeToId = identity }
                    |> String.contains "fillcolor=\"pink\""
                    |> Expect.equal True
                    |> Expect.onFail "We should have seen `fillcolor=pink` string"
        , Test.test "Direction of arrows" <|
            \() ->
                Sequential [ Node "0", Node "1" ]
                    |> Flow.toDot { nodeToId = identity }
                    |> String.contains "\"0\" -> \"1\""
                    |> Expect.equal True
                    |> Expect.onFail "We want dependencies to point to dependants"
        ]


breadthFirstFold : Test
breadthFirstFold =
    Test.describe "breadthFirstFold"
        [ Test.test "Node" <|
            \_ ->
                Node 0
                    |> Flow.breadthFirstFold (::) []
                    |> Expect.equal [ 0 ]
        , Test.test "Sequential" <|
            \_ ->
                Sequential [ Node 0, Node 1 ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1 ]
        , Test.test "Parallel" <|
            \_ ->
                Parallel [ Node 0, Node 1 ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1 ]
        , Test.test "Condition" <|
            \_ ->
                Condition 0
                    { trueSeq = [ Node 1, Node 2 ]
                    , falseSeq = [ Node 3, Node 4 ]
                    }
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1, 2, 3, 4 ]
        , Test.test "Top-down then left-right" <|
            \_ ->
                Sequential
                    [ Node 0
                    , Parallel
                        [ Sequential [ Node 1, Node 3 ]
                        , Sequential [ Node 2, Node 4 ]
                        ]
                    , Node 5
                    ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1, 2, 3, 4, 5 ]
        , Test.test "Nested parallel" <|
            \_ ->
                Sequential
                    [ Node 0
                    , Parallel
                        [ Sequential [ Node 1, Node 3, Node 6 ]
                        , Sequential
                            [ Node 2
                            , Parallel
                                [ Node 4
                                , Node 5
                                ]
                            ]
                        ]
                    , Node 7
                    ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1, 2, 3, 4, 5, 6, 7 ]
        , Test.test "Everything in true branch before false branch" <|
            \_ ->
                Sequential
                    [ Node 0
                    , Condition 1
                        { trueSeq =
                            [ Parallel
                                [ Sequential [ Node 2, Node 4, Node 7 ]
                                , Sequential
                                    [ Node 3
                                    , Parallel
                                        [ Node 5
                                        , Node 6
                                        ]
                                    ]
                                ]
                            ]
                        , falseSeq =
                            [ Parallel
                                [ Sequential [ Node 8, Node 10, Node 13 ]
                                , Sequential
                                    [ Node 9
                                    , Parallel
                                        [ Node 11
                                        , Node 12
                                        ]
                                    ]
                                ]
                            ]
                        }
                    , Node 14
                    ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
        , Test.fuzz emptyFlowFuzzer "Empty flow" <|
            \emptyFlow ->
                emptyFlow
                    |> Flow.breadthFirstFold (::) []
                    |> Expect.equal []
        , Test.fuzz emptyFlowFuzzer "Sequential with empty flow" <|
            \emptyFlow ->
                Sequential [ Node 0, emptyFlow, Node 1 ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1 ]
        , Test.fuzz flowFuzzer "Same set of nodes as Flow.nodes" <|
            \flow ->
                flow
                    |> Flow.breadthFirstFold (::) []
                    |> Set.fromList
                    |> Expect.equal
                        (flow
                            |> Flow.nodes
                            |> List.map Tuple.first
                            |> Set.fromList
                        )
        , Test.fuzz flowFuzzer "Invariant under `optimize`" <|
            \flow ->
                flow
                    |> Flow.breadthFirstFold (::) []
                    |> Expect.equal
                        (flow
                            |> Flow.optimize
                            |> Flow.breadthFirstFold (::) []
                        )
        , Test.test "Invariant under `optimize`: Found case 1" <|
            \_ ->
                Parallel
                    [ Sequential
                        [ Sequential []
                        , Node 0
                        ]
                    , Node 1
                    ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1 ]
        , Test.test "Invariant under `optimize`: Found case 2" <|
            \_ ->
                Parallel
                    [ Sequential
                        [ Sequential [ Node 0, Node 2 ]
                        , Sequential [ Node 4 ]
                        ]
                    , Sequential [ Node 1, Node 3 ]
                    ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1, 2, 3, 4 ]
        , Test.test "Invariant under `optimize`: Found case 3" <|
            \_ ->
                Parallel
                    [ Sequential
                        [ Sequential [ Node 0, Node 2 ]
                        , Condition 4
                            { falseSeq = []
                            , trueSeq = []
                            }
                        ]
                    , Sequential [ Node 1, Node 3 ]
                    ]
                    |> Flow.breadthFirstFold (::) []
                    |> List.reverse
                    |> Expect.equal [ 0, 1, 2, 3, 4 ]
        ]



-- FUZZERS


shortListFuzzer : Fuzzer a -> Fuzzer (List a)
shortListFuzzer childFuzzer =
    Fuzz.listOfLengthBetween 0 4 childFuzzer


nonemptyShortListFuzzer : Fuzzer a -> Fuzzer (List a)
nonemptyShortListFuzzer childFuzzer =
    Fuzz.listOfLengthBetween 1 4 childFuzzer


withIndex : Fuzzer (List a) -> Fuzzer ( List a, Int )
withIndex listFuzzer =
    listFuzzer
        |> Fuzz.andThen
            (\xs ->
                Fuzz.intRange 0 (List.length xs - 1)
                    |> Fuzz.map (\i -> ( xs, i ))
            )


flowFuzzer : Fuzzer (Flow Int)
flowFuzzer =
    -- We're limiting the recursion to max 4 levels of depth.
    -- Each level also can have progressively less seq/par children.
    flowFuzzerOfMaxDepth 4


flowFuzzerOfMaxDepth : Int -> Fuzzer (Flow Int)
flowFuzzerOfMaxDepth userGivenMaxDepth =
    let
        initDepth =
            (userGivenMaxDepth - 1) |> max 0

        initDepthFloat =
            toFloat initDepth

        leaf : Fuzzer (Flow Int)
        leaf =
            Fuzz.map Node (Fuzz.intAtLeast 0)

        go : Int -> Fuzzer (Flow Int)
        go maxDepth =
            if maxDepth <= 0 then
                leaf

            else
                let
                    child =
                        go (maxDepth - 1)

                    everShorterListFuzzer =
                        Fuzz.listOfLengthBetween 0 (floor (6 * (toFloat maxDepth / initDepthFloat))) child
                in
                Fuzz.oneOf
                    [ leaf
                    , Fuzz.map Sequential everShorterListFuzzer
                    , Fuzz.map Parallel everShorterListFuzzer
                    , Fuzz.map3 (\n t f -> Condition n { trueSeq = t, falseSeq = f })
                        (Fuzz.intAtLeast 0)
                        everShorterListFuzzer
                        everShorterListFuzzer
                    ]
    in
    go initDepth


{-| Like flowFuzzer but only produces (recursively) empty flows
-}
emptyFlowFuzzer : Fuzzer (Flow a)
emptyFlowFuzzer =
    -- We're limiting the recursion to max 4 levels of depth.
    -- Each level also can have progressively less seq/par children.
    let
        initDepth =
            3

        initDepthFloat =
            toFloat initDepth

        shallow : Fuzzer (Flow a)
        shallow =
            Fuzz.oneOf
                [ Fuzz.constant (Sequential [])
                , Fuzz.constant (Parallel [])
                ]

        go : Int -> Fuzzer (Flow a)
        go maxDepth =
            if maxDepth <= 0 then
                shallow

            else
                let
                    child =
                        go (maxDepth - 1)

                    everShorterListFuzzer =
                        Fuzz.listOfLengthBetween 0 (floor (6 * (toFloat maxDepth / initDepthFloat))) child
                in
                Fuzz.oneOf
                    [ shallow
                    , Fuzz.map Sequential everShorterListFuzzer
                    , Fuzz.map Parallel everShorterListFuzzer
                    ]
    in
    go initDepth


pathFuzzer : Fuzzer (List Step)
pathFuzzer =
    Fuzz.listOfLengthBetween 0 4 stepFuzzer


stepFuzzer : Fuzzer Step
stepFuzzer =
    Fuzz.oneOf
        [ Fuzz.map InSequential (Fuzz.intRange 0 4)
        , Fuzz.map InParallel (Fuzz.intRange 0 4)
        , Fuzz.map InConditionTrue (Fuzz.intRange 0 4)
        , Fuzz.map InConditionFalse (Fuzz.intRange 0 4)
        ]


stepConstructorFuzzer : Fuzzer (Int -> Step)
stepConstructorFuzzer =
    Fuzz.oneOfValues
        [ InSequential
        , InParallel
        , InConditionTrue
        , InConditionFalse
        ]


flowWithUniqueIdsFuzzer : Fuzzer (Flow Int)
flowWithUniqueIdsFuzzer =
    flowFuzzer
        |> Fuzz.filter
            (\flow ->
                let
                    ids =
                        flow |> Flow.nodes |> List.map Tuple.first

                    uniqueIds =
                        Set.fromList ids
                in
                List.length ids == Set.size uniqueIds
            )

module Unit.Flow.Operation exposing (suite)

import Expect exposing (Expectation)
import Flow exposing (DependencyChange, Flow(..), Step(..))
import Flow.Operation exposing (Operation(..))
import Fuzz exposing (Fuzzer)
import Set
import Test exposing (Test)
import Unit.Flow


suite : Test
suite =
    Test.describe "Flow.Operation"
        [ applyAt
        ]


applyAt : Test
applyAt =
    Test.describe "applyAt"
        [ outsideBounds
        , addBefore
        , addAfter
        , addParallelBelow
        , reattachFalseInsideCondition
        , reattachFalseOutsideCondition
        , remove
        ]


expectEqualAfterOptimizing : Maybe ( Flow node, List DependencyChange ) -> Maybe ( Flow node, List DependencyChange ) -> Expectation
expectEqualAfterOptimizing mExpected mActual =
    let
        fn maybe =
            maybe
                |> Maybe.map
                    (Tuple.mapBoth
                        Flow.optimize
                        (List.map
                            (\d ->
                                ( d.task
                                , List.sort <| List.map (\dd -> ( dd.id, dd.branch )) d.newDeps
                                , List.sort <| List.map (\dd -> ( dd.id, dd.branch )) d.removeDeps
                                )
                            )
                            >> List.sort
                        )
                    )
    in
    fn mActual
        |> Expect.equal (fn mExpected)


opCfg : Flow.Operation.Config Int
opCfg =
    { nodeToId = id
    }


id : Int -> String
id n =
    n
        |> String.fromInt


outsideBounds : Test
outsideBounds =
    Test.fuzz3
        Unit.Flow.flowFuzzer
        Unit.Flow.pathFuzzer
        (opFuzzer Fuzz.int)
        "Applying any operation outside bounds doesn't change the flow (except for empty collection and index 0)"
    <|
        \flow path op ->
            case Flow.navigateTo path flow of
                Nothing ->
                    let
                        isAdd =
                            case op of
                                AddBefore _ _ ->
                                    True

                                AddAfter _ _ ->
                                    True

                                AddParallelBelow _ _ ->
                                    True

                                ReattachFalseInsideCondition _ ->
                                    False

                                ReattachFalseOutsideCondition _ ->
                                    False

                                Remove ->
                                    False

                        lastPathIndexIs0 =
                            case List.reverse path of
                                (InSequential n) :: _ ->
                                    n == 0

                                (InParallel n) :: _ ->
                                    n == 0

                                (InConditionTrue n) :: _ ->
                                    n == 0

                                (InConditionFalse n) :: _ ->
                                    n == 0

                                [] ->
                                    False
                    in
                    if isAdd && lastPathIndexIs0 then
                        Expect.pass

                    else
                        Flow.Operation.applyAt opCfg path op flow
                            |> Expect.equal Nothing

                Just _ ->
                    Expect.pass



{-
   The test names here are a stringified Flow AST:

   0 = Node 0
   [0,1] = Sequential [Node 0, Node 1]
   {0,1} = Parallel [Node 0, Node 1]
   0?(ts,fs) = Condition 0 {trueSeq = ts, falseSeq = fs}

   The `>` character means current focus.

   We'll be adding node 99 in all these scenarios.

   The node 100 is the Finalize task.

   -> means "transforms into exactly"
   ~> means "transforms into (optimized)"

   Thus ">0 -> [99,0]" in `addBefore` test means we'll addBefore the node 0
   and end up with Sequential [Node 99, Node 0].

-}


addedNode : Int
addedNode =
    99


flowFuzzerWithoutAddedNodeId : Fuzzer (Flow Int)
flowFuzzerWithoutAddedNodeId =
    Unit.Flow.flowFuzzer
        -- If we randomly generate the added node integer, let's just throw it away and try again.
        |> Fuzz.filter (Flow.nodes >> List.any (Tuple.first >> (==) addedNode) >> not)


addBefore : Test
addBefore =
    Test.describe "AddBefore"
        [ Test.describe "isCondition = False" <|
            let
                addBeforeAt path rootFlow =
                    Flow.Operation.applyAt
                        opCfg
                        path
                        (AddBefore { isCondition = False } addedNode)
                        rootFlow
            in
            [ Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Always returns True" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            addBeforeAt path flow
                                |> Expect.notEqual Nothing
            , Test.test ">0 -> [99,0]" <|
                \() ->
                    addBeforeAt [] (Node 0)
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node addedNode, Node 0 ]
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[0,>1] -> [0,99,1] (removes deps!)" <|
                \() ->
                    addBeforeAt
                        [ InSequential 1 ]
                        (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Node addedNode, Node 1 ]
                                , [ { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 0 } ]
                                    }
                                  , { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">[0,1] ~> [99,0,1] " <|
                \() ->
                    addBeforeAt
                        []
                        (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node addedNode, Node 0, Node 1 ]
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">{0,1} -> [99,{0,1}]" <|
                \() ->
                    addBeforeAt
                        []
                        (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node addedNode, Parallel [ Node 0, Node 1 ] ]
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "{0,>1} -> {0,[99,1]}" <|
                \() ->
                    addBeforeAt
                        [ InParallel 1 ]
                        (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Parallel [ Node 0, Sequential [ Node addedNode, Node 1 ] ]
                                , [ { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[0,{1,>2}] -> [0,{1,[99,2]}]" <|
                \() ->
                    addBeforeAt
                        [ InSequential 1, InParallel 1 ]
                        (Sequential [ Node 0, Parallel [ Node 1, Node 2 ] ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Parallel [ Node 1, Sequential [ Node addedNode, Node 2 ] ] ]
                                , [ { task = id 2
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 0 } ]
                                    }
                                  , { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">0?([1],[2]) -> [99,0?([1],[2])]" <|
                \() ->
                    addBeforeAt
                        []
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node addedNode, Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] } ]
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "0?([>1],[2]) -> 0?([99,1],[2])]" <|
                \() ->
                    addBeforeAt
                        [ InConditionTrue 0 ]
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Node addedNode, Node 1 ], falseSeq = [ Node 2 ] }
                                , [ { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 0 } ]
                                    }
                                  , { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "0?([1],[>2]) -> 0?([1],[99,2])]" <|
                \() ->
                    addBeforeAt
                        [ InConditionFalse 0 ]
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node addedNode, Node 2 ] }
                                , [ { task = id 2
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 1, id = id 0 } ]
                                    }
                                  , { task = id addedNode
                                    , newDeps = [ { branch = 1, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.fuzz2
                Unit.Flow.pathFuzzer
                flowFuzzerWithoutAddedNodeId
                "Nodes stay the same, except for the added one"
              <|
                \path flow ->
                    case addBeforeAt path flow of
                        Nothing ->
                            Expect.pass

                        Just ( newFlow, _ ) ->
                            Set.diff
                                (Flow.nodes newFlow |> List.map Tuple.first |> Set.fromList)
                                (Flow.nodes flow |> List.map Tuple.first |> Set.fromList)
                                |> Expect.equalSets (Set.singleton addedNode)
            , Test.test "Empty case: 0?([>],[]) -> 0?([99],[])" <|
                \() ->
                    addBeforeAt [ InConditionTrue 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Node addedNode ], falseSeq = [] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: 0?([],[>]) -> 0?([],[99])" <|
                \() ->
                    addBeforeAt [ InConditionFalse 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [], falseSeq = [ Node addedNode ] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 1, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: [>] -> [99]" <|
                \() ->
                    addBeforeAt [ InSequential 0 ] (Sequential [])
                        |> expectEqualAfterOptimizing (Just ( Sequential [ Node addedNode ], [] ))
            , Test.test "Empty case: {>} -> {99}" <|
                \() ->
                    addBeforeAt [ InParallel 0 ] (Parallel [])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Node addedNode ], [] ))
            ]
        , Test.describe "isCondition = True" <|
            let
                addBeforeAt path rootFlow =
                    Flow.Operation.applyAt
                        opCfg
                        path
                        (AddBefore { isCondition = True } addedNode)
                        rootFlow
            in
            [ Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Always returns True" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            addBeforeAt path flow
                                |> Expect.notEqual Nothing
            , Test.test ">0 -> 99?([0],[])" <|
                \() ->
                    addBeforeAt [] (Node 0)
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition addedNode { trueSeq = [ Node 0 ], falseSeq = [] }
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">[0,1] -> 99?([0,1],[])" <|
                \() ->
                    addBeforeAt [] (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition addedNode { trueSeq = [ Node 0, Node 1 ], falseSeq = [] }
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">{0,1} -> 99?([{0,1}],[])" <|
                \() ->
                    addBeforeAt [] (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition addedNode { trueSeq = [ Parallel [ Node 0, Node 1 ] ], falseSeq = [] }
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">0?([1],[2]) -> 99?([0?([1],[2])],[])" <|
                \() ->
                    addBeforeAt [] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition addedNode { trueSeq = [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] } ], falseSeq = [] }
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[>0,1,2] ~> 99?([0,1,2],[])" <|
                \() ->
                    addBeforeAt [ InSequential 0 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition addedNode { trueSeq = [ Node 0, Node 1, Node 2 ], falseSeq = [] }
                                , [ { task = id 0
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[0,>1,2] -> [0,99?([1,2],[])]" <|
                \() ->
                    addBeforeAt [ InSequential 1 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Condition addedNode { trueSeq = [ Node 1, Node 2 ], falseSeq = [] } ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 0 } ]
                                    }
                                  ]
                                )
                            )
            , Test.test "[0,1,>2] -> [0,1,99?([2],[])]" <|
                \() ->
                    addBeforeAt [ InSequential 2 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Node 1, Condition addedNode { trueSeq = [ Node 2 ], falseSeq = [] } ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 2
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 1 } ]
                                    }
                                  ]
                                )
                            )
            , Test.test "[{[0,>1,2],3},4] -> [{[0,99?([1,2],[])],3},4]" <|
                \() ->
                    addBeforeAt
                        [ InSequential 0, InParallel 0, InSequential 1 ]
                        (Sequential
                            [ Parallel
                                [ Sequential [ Node 0, Node 1, Node 2 ]
                                , Node 3
                                ]
                            , Node 4
                            ]
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Parallel
                                        [ Sequential [ Node 0, Condition addedNode { trueSeq = [ Node 1, Node 2 ], falseSeq = [] } ]
                                        , Node 3
                                        ]
                                    , Node 4
                                    ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 0 } ]
                                    }
                                  , { task = id 4
                                    , newDeps = [ { branch = 1, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[{0,1},>2] -> [{0,1},99?([2],[])]" <|
                \() ->
                    addBeforeAt [ InSequential 1 ] (Sequential [ Parallel [ Node 0, Node 1 ], Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Node 1 ], Condition addedNode { trueSeq = [ Node 2 ], falseSeq = [] } ]
                                , [ { task = id 2
                                    , newDeps = [ { id = id addedNode, branch = 0 } ]
                                    , removeDeps =
                                        [ { id = id 0, branch = 0 }
                                        , { id = id 1, branch = 0 }
                                        ]
                                    }
                                  , { task = id addedNode
                                    , newDeps =
                                        [ { id = id 0, branch = 0 }
                                        , { id = id 1, branch = 0 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "0?([>1,2],[3]) -> 0?([99?([1,2], [])],[3])" <|
                \() ->
                    addBeforeAt [ InConditionTrue 0 ]
                        (Condition 0
                            { trueSeq = [ Node 1, Node 2 ]
                            , falseSeq = [ Node 3 ]
                            }
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0
                                    { trueSeq =
                                        [ Condition addedNode
                                            { trueSeq = [ Node 1, Node 2 ]
                                            , falseSeq = []
                                            }
                                        ]
                                    , falseSeq = [ Node 3 ]
                                    }
                                , [ { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 0 } ]
                                    }
                                  , { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "0?([1],[>2,3]) -> 0?([1],[99?([2,3], [])])" <|
                \() ->
                    addBeforeAt [ InConditionFalse 0 ]
                        (Condition 0
                            { trueSeq = [ Node 1 ]
                            , falseSeq = [ Node 2, Node 3 ]
                            }
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0
                                    { trueSeq = [ Node 1 ]
                                    , falseSeq =
                                        [ Condition addedNode
                                            { trueSeq = [ Node 2, Node 3 ]
                                            , falseSeq = []
                                            }
                                        ]
                                    }
                                , [ { task = id 2
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 1, id = id 0 } ]
                                    }
                                  , { task = id addedNode
                                    , newDeps = [ { branch = 1, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Created Condition always has empty falseSeq" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            case addBeforeAt path flow of
                                Just ( newFlow, _ ) ->
                                    Flow.find
                                        (\f ->
                                            case f of
                                                Condition node { falseSeq } ->
                                                    if node == addedNode then
                                                        List.isEmpty falseSeq

                                                    else
                                                        False

                                                _ ->
                                                    False
                                        )
                                        newFlow
                                        |> Expect.notEqual Nothing

                                _ ->
                                    Expect.fail "Should have been Just"
            , Test.fuzz2
                Unit.Flow.pathFuzzer
                flowFuzzerWithoutAddedNodeId
                "Nodes stay the same, except for the added one"
              <|
                \path flow ->
                    case addBeforeAt path flow of
                        Nothing ->
                            Expect.pass

                        Just ( newFlow, _ ) ->
                            Set.diff
                                (Flow.nodes newFlow |> List.map Tuple.first |> Set.fromList)
                                (Flow.nodes flow |> List.map Tuple.first |> Set.fromList)
                                |> Expect.equalSets (Set.singleton addedNode)
            , Test.test "Empty case: 0?([>],[]) -> 0?([99?([],[])],[])" <|
                \() ->
                    addBeforeAt [ InConditionTrue 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Condition addedNode { trueSeq = [], falseSeq = [] } ], falseSeq = [] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: 0?([],[>]) -> 0?([],[99?([],[])])" <|
                \() ->
                    addBeforeAt [ InConditionFalse 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [], falseSeq = [ Condition addedNode { trueSeq = [], falseSeq = [] } ] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 1, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: [>] -> [99?([],[])]" <|
                \() ->
                    addBeforeAt [ InSequential 0 ] (Sequential [])
                        |> expectEqualAfterOptimizing (Just ( Sequential [ Condition addedNode { trueSeq = [], falseSeq = [] } ], [] ))
            , Test.test "Empty case: {>} -> {99?([],[])}" <|
                \() ->
                    addBeforeAt [ InParallel 0 ] (Parallel [])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Condition addedNode { trueSeq = [], falseSeq = [] } ], [] ))
            ]
        ]


addAfter : Test
addAfter =
    Test.describe "AddAfter"
        [ Test.describe "isCondition = False" <|
            let
                addAfterAt path rootFlow =
                    Flow.Operation.applyAt
                        opCfg
                        path
                        (AddAfter { isCondition = False } addedNode)
                        rootFlow
            in
            [ Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Always returns True" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            addAfterAt path flow
                                |> Expect.notEqual Nothing
            , Test.test ">0 -> [0,99]" <|
                \() ->
                    addAfterAt [] (Node 0)
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Node addedNode ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[>0,1] -> [0,99,1] (removes deps!)" <|
                \() ->
                    addAfterAt
                        [ InSequential 0 ]
                        (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Node addedNode, Node 1 ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 0 } ]
                                    }
                                  ]
                                )
                            )
            , Test.test ">[0,1] ~> [0,1,99] " <|
                \() ->
                    addAfterAt
                        []
                        (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Node 1, Node addedNode ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">{0,1} -> [{0,1},99]" <|
                \() ->
                    addAfterAt
                        []
                        (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Node 1 ], Node addedNode ]
                                , [ { task = id addedNode
                                    , newDeps =
                                        [ { branch = 0, id = id 0 }
                                        , { branch = 0, id = id 1 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "{0,>1} -> {0,[1,99]}" <|
                \() ->
                    addAfterAt
                        [ InParallel 1 ]
                        (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Parallel [ Node 0, Sequential [ Node 1, Node addedNode ] ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[0,{1,>2}] -> [0,{1,[2,99]}]" <|
                \() ->
                    addAfterAt
                        [ InSequential 1, InParallel 1 ]
                        (Sequential [ Node 0, Parallel [ Node 1, Node 2 ] ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Parallel [ Node 1, Sequential [ Node 2, Node addedNode ] ] ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 2 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[{0,>1},2] -> [{0,[1,99]},2]" <|
                \() ->
                    addAfterAt
                        [ InSequential 0, InParallel 1 ]
                        (Sequential [ Parallel [ Node 0, Node 1 ], Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Sequential [ Node 1, Node addedNode ] ], Node 2 ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 2
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 1 } ]
                                    }
                                  ]
                                )
                            )
            , Test.test ">0?([1],[2]) -> [0?([1],[2]),99]" <|
                \() ->
                    addAfterAt
                        []
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] }, Node addedNode ]
                                , [ { task = id addedNode
                                    , newDeps =
                                        [ { branch = 0, id = id 1 }
                                        , { branch = 0, id = id 2 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "0?([>1],[2]) -> 0?([1,99],[2])]" <|
                \() ->
                    addAfterAt
                        [ InConditionTrue 0 ]
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Node 1, Node addedNode ], falseSeq = [ Node 2 ] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "0?([1],[>2]) -> 0?([1],[2,99])]" <|
                \() ->
                    addAfterAt
                        [ InConditionFalse 0 ]
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2, Node addedNode ] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 2 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Add sequentially after condition" <|
                \() ->
                    addAfterAt
                        [ InSequential 0 ]
                        (Sequential
                            [ Condition 0
                                { trueSeq = [ Node 1 ]
                                , falseSeq = [ Node 2, Node 3 ]
                                }
                            , Node 4
                            ]
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Condition 0
                                        { trueSeq = [ Node 1 ]
                                        , falseSeq = [ Node 2, Node 3 ]
                                        }
                                    , Node addedNode
                                    , Node 4
                                    ]
                                , [ { task = id 4
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps =
                                        [ { branch = 0, id = id 1 }
                                        , { branch = 0, id = id 3 }
                                        ]
                                    }
                                  , { task = id addedNode
                                    , newDeps =
                                        [ { branch = 0, id = id 1 }
                                        , { branch = 0, id = id 3 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.fuzz2
                Unit.Flow.pathFuzzer
                flowFuzzerWithoutAddedNodeId
                "Nodes stay the same, except for the added one"
              <|
                \path flow ->
                    case addAfterAt path flow of
                        Nothing ->
                            Expect.pass

                        Just ( newFlow, _ ) ->
                            Set.diff
                                (Flow.nodes newFlow |> List.map Tuple.first |> Set.fromList)
                                (Flow.nodes flow |> List.map Tuple.first |> Set.fromList)
                                |> Expect.equalSets (Set.singleton addedNode)
            , Test.test "Empty case: 0?([>],[]) -> 0?([99],[])" <|
                \() ->
                    addAfterAt [ InConditionTrue 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Node addedNode ], falseSeq = [] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: 0?([],[>]) -> 0?([],[99])" <|
                \() ->
                    addAfterAt [ InConditionFalse 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [], falseSeq = [ Node addedNode ] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 1, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: [0, 1?([],[>])] -> [0, 1?([],[99])]" <|
                \() ->
                    addAfterAt [ InSequential 1, InConditionFalse 0 ]
                        (Sequential
                            [ Node 0
                            , Condition 1 { trueSeq = [], falseSeq = [] }
                            ]
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Node 0
                                    , Condition 1 { trueSeq = [], falseSeq = [ Node addedNode ] }
                                    ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 1, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: [0, 1?([>],[])] -> [0, 1?([99],[])]" <|
                \() ->
                    addAfterAt [ InSequential 1, InConditionTrue 0 ]
                        (Sequential
                            [ Node 0
                            , Condition 1 { trueSeq = [], falseSeq = [] }
                            ]
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Node 0
                                    , Condition 1 { trueSeq = [ Node addedNode ], falseSeq = [] }
                                    ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: [0, [>]] ~> [0, 99]" <|
                \() ->
                    addAfterAt [ InSequential 1, InSequential 0 ]
                        (Sequential
                            [ Node 0
                            , Sequential []
                            ]
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Node 0
                                    , Node addedNode
                                    ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: [0, {>}] ~> [0, 99]" <|
                \() ->
                    addAfterAt [ InSequential 1, InParallel 0 ]
                        (Sequential
                            [ Node 0
                            , Parallel []
                            ]
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Node 0
                                    , Node addedNode
                                    ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: [>] -> [99]" <|
                \() ->
                    addAfterAt [ InSequential 0 ] (Sequential [])
                        |> expectEqualAfterOptimizing (Just ( Sequential [ Node addedNode ], [] ))
            , Test.test "Empty case: {>} -> {99}" <|
                \() ->
                    addAfterAt [ InParallel 0 ] (Parallel [])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Node addedNode ], [] ))
            ]
        , Test.describe "isCondition = True" <|
            let
                addAfterAt path rootFlow =
                    Flow.Operation.applyAt
                        opCfg
                        path
                        (AddAfter { isCondition = True } addedNode)
                        rootFlow
            in
            [ Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Always returns True" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            addAfterAt path flow
                                |> Expect.notEqual Nothing
            , Test.test ">0 -> [0, 99?([],[])]" <|
                \() ->
                    addAfterAt [] (Node 0)
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Condition addedNode { trueSeq = [], falseSeq = [] } ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">[0,1] ~> [0,1,99?([],[])]" <|
                \() ->
                    addAfterAt [] (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Node 1, Condition addedNode { trueSeq = [], falseSeq = [] } ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">{0,1} -> [{0,1},99?([],[])]" <|
                \() ->
                    addAfterAt [] (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Node 1 ], Condition addedNode { trueSeq = [], falseSeq = [] } ]
                                , [ { task = id addedNode
                                    , newDeps =
                                        [ { branch = 0, id = id 0 }
                                        , { branch = 0, id = id 1 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">0?([1],[2]) -> [0?([1],[2]), 99?([],[])]" <|
                \() ->
                    addAfterAt [] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] }
                                    , Condition addedNode { trueSeq = [], falseSeq = [] }
                                    ]
                                , [ { task = id addedNode
                                    , newDeps =
                                        [ { branch = 0, id = id 1 }
                                        , { branch = 0, id = id 2 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">0?([1],[]) -> [0?([1],[]), 99?([],[])]" <|
                \() ->
                    addAfterAt [] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [] }
                                    , Condition addedNode { trueSeq = [], falseSeq = [] }
                                    ]
                                , [ { task = id addedNode
                                    , newDeps =
                                        [ { branch = 0, id = id 1 }
                                        , { branch = 1, id = id 0 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">0?([],[1]) -> [0?([],[1]), 99?([],[])]" <|
                \() ->
                    addAfterAt [] (Condition 0 { trueSeq = [], falseSeq = [ Node 1 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Condition 0 { trueSeq = [], falseSeq = [ Node 1 ] }
                                    , Condition addedNode { trueSeq = [], falseSeq = [] }
                                    ]
                                , [ { task = id addedNode
                                    , newDeps =
                                        [ { branch = 0, id = id 1 }
                                        , { branch = 0, id = id 0 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[>0,1,2] ~> [0, 99?([1,2],[])]" <|
                \() ->
                    addAfterAt [ InSequential 0 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Condition addedNode { trueSeq = [ Node 1, Node 2 ], falseSeq = [] } ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 0 } ]
                                    }
                                  ]
                                )
                            )
            , Test.test "[0,>1,2] -> [0,1,99?([2],[])]" <|
                \() ->
                    addAfterAt [ InSequential 1 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Node 1, Condition addedNode { trueSeq = [ Node 2 ], falseSeq = [] } ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 2
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 1 } ]
                                    }
                                  ]
                                )
                            )
            , Test.test "[0,1,>2] -> [0,1,2,99?([],[])]" <|
                \() ->
                    addAfterAt [ InSequential 2 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Node 1, Node 2, Condition addedNode { trueSeq = [], falseSeq = [] } ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 2 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[{[0,>1,2],3},4] -> [{[0,1,99?([2],[])],3},4]" <|
                \() ->
                    addAfterAt
                        [ InSequential 0, InParallel 0, InSequential 1 ]
                        (Sequential
                            [ Parallel
                                [ Sequential [ Node 0, Node 1, Node 2 ]
                                , Node 3
                                ]
                            , Node 4
                            ]
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Parallel
                                        [ Sequential [ Node 0, Node 1, Condition addedNode { trueSeq = [ Node 2 ], falseSeq = [] } ]
                                        , Node 3
                                        ]
                                    , Node 4
                                    ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 1 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 2
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = [ { branch = 0, id = id 1 } ]
                                    }
                                  , { task = id 4
                                    , newDeps = [ { branch = 1, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[>{0,1},2] -> [{0,1},99?([2],[])]" <|
                \() ->
                    addAfterAt [ InSequential 0 ] (Sequential [ Parallel [ Node 0, Node 1 ], Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Node 1 ], Condition addedNode { trueSeq = [ Node 2 ], falseSeq = [] } ]
                                , [ { task = id 2
                                    , newDeps = [ { id = id addedNode, branch = 0 } ]
                                    , removeDeps =
                                        [ { id = id 0, branch = 0 }
                                        , { id = id 1, branch = 0 }
                                        ]
                                    }
                                  , { task = id addedNode
                                    , newDeps =
                                        [ { id = id 0, branch = 0 }
                                        , { id = id 1, branch = 0 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Created Condition always has empty falseSeq" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            case addAfterAt path flow of
                                Just ( newFlow, _ ) ->
                                    Flow.find
                                        (\f ->
                                            case f of
                                                Condition node { falseSeq } ->
                                                    if node == addedNode then
                                                        List.isEmpty falseSeq

                                                    else
                                                        False

                                                _ ->
                                                    False
                                        )
                                        newFlow
                                        |> Expect.notEqual Nothing

                                _ ->
                                    Expect.fail "Should have been Just"
            , Test.fuzz2
                Unit.Flow.pathFuzzer
                flowFuzzerWithoutAddedNodeId
                "Nodes stay the same, except for the added one"
              <|
                \path flow ->
                    case addAfterAt path flow of
                        Nothing ->
                            Expect.pass

                        Just ( newFlow, _ ) ->
                            Set.diff
                                (Flow.nodes newFlow |> List.map Tuple.first |> Set.fromList)
                                (Flow.nodes flow |> List.map Tuple.first |> Set.fromList)
                                |> Expect.equalSets (Set.singleton addedNode)
            , Test.test "Empty case: 0?([>],[]) -> 0?([99?([],[])],[])" <|
                \() ->
                    addAfterAt [ InConditionTrue 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Condition addedNode { trueSeq = [], falseSeq = [] } ], falseSeq = [] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: 0?([],[>]) -> 0?([],[99?([],[])])" <|
                \() ->
                    addAfterAt [ InConditionFalse 0 ] (Condition 0 { trueSeq = [], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [], falseSeq = [ Condition addedNode { trueSeq = [], falseSeq = [] } ] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 1, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "Empty case: [>] -> [99?([],[])]" <|
                \() ->
                    addAfterAt [ InSequential 0 ] (Sequential [])
                        |> expectEqualAfterOptimizing (Just ( Sequential [ Condition addedNode { trueSeq = [], falseSeq = [] } ], [] ))
            , Test.test "Empty case: {>} -> {99?([],[])}" <|
                \() ->
                    addAfterAt [ InParallel 0 ] (Parallel [])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Condition addedNode { trueSeq = [], falseSeq = [] } ], [] ))
            ]
        ]


addParallelBelow : Test
addParallelBelow =
    Test.describe "AddParallelBelow"
        [ Test.describe "isCondition = False" <|
            let
                addParallelAt path rootFlow =
                    Flow.Operation.applyAt
                        opCfg
                        path
                        (AddParallelBelow { isCondition = False } addedNode)
                        rootFlow
            in
            [ Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Always returns True" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            addParallelAt path flow
                                |> Expect.notEqual Nothing
            , Test.test ">0 -> {0,99}" <|
                \() ->
                    addParallelAt [] (Node 0)
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Node 0, Node addedNode ], [] ))
            , Test.test "[>0,1] -> [{0,99},1]" <|
                \() ->
                    addParallelAt
                        [ InSequential 0 ]
                        (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Node addedNode ], Node 1 ]
                                , [ { task = id 1
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">[0,1] -> {[0,1],99} " <|
                \() ->
                    addParallelAt
                        []
                        (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Sequential [ Node 0, Node 1 ], Node addedNode ], [] ))
            , Test.test ">{0,1} ~> {0,1,99}" <|
                \() ->
                    addParallelAt
                        []
                        (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Node 0, Node 1, Node addedNode ], [] ))
            , Test.test "{>0,1} ~> {0,99,1}" <|
                \() ->
                    addParallelAt
                        [ InParallel 0 ]
                        (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Node 0, Node addedNode, Node 1 ], [] ))
            , Test.test "{0,>1} ~> {0,1,99}" <|
                \() ->
                    addParallelAt
                        [ InParallel 1 ]
                        (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Node 0, Node 1, Node addedNode ], [] ))
            , Test.test "[0,{1,>2}] ~> [0,{1,2,99}]" <|
                \() ->
                    addParallelAt
                        [ InSequential 1, InParallel 1 ]
                        (Sequential [ Node 0, Parallel [ Node 1, Node 2 ] ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Parallel [ Node 1, Node 2, Node addedNode ] ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[{0,>1},2] ~> [{0,1,99},2]" <|
                \() ->
                    addParallelAt
                        [ InSequential 0, InParallel 1 ]
                        (Sequential [ Parallel [ Node 0, Node 1 ], Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Node 1, Node addedNode ], Node 2 ]
                                , [ { task = id 2
                                    , newDeps = [ { branch = 0, id = id addedNode } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[{0,1},>{2,3}] ~> [{0,1},{2,3,99}]" <|
                \() ->
                    addParallelAt
                        [ InSequential 1 ]
                        (Sequential [ Parallel [ Node 0, Node 1 ], Parallel [ Node 2, Node 3 ] ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Node 1 ], Parallel [ Node 2, Node 3, Node addedNode ] ]
                                , [ { task = id addedNode
                                    , newDeps =
                                        [ { branch = 0, id = id 0 }
                                        , { branch = 0, id = id 1 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test ">0?([1],[2]) -> {0?([1],[2]),99}" <|
                \() ->
                    addParallelAt
                        []
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] }, Node addedNode ], [] ))
            , Test.test "0?([>1],[2]) -> 0?([{1,99}],[2])]" <|
                \() ->
                    addParallelAt
                        [ InConditionTrue 0 ]
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Parallel [ Node 1, Node addedNode ] ], falseSeq = [ Node 2 ] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "0?([1],[>2]) -> 0?([1],[{2,99}])]" <|
                \() ->
                    addParallelAt
                        [ InConditionFalse 0 ]
                        (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Parallel [ Node 2, Node addedNode ] ] }
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 1, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.fuzz2
                Unit.Flow.pathFuzzer
                flowFuzzerWithoutAddedNodeId
                "Nodes stay the same, except for the added one"
              <|
                \path flow ->
                    case addParallelAt path flow of
                        Nothing ->
                            Expect.pass

                        Just ( newFlow, _ ) ->
                            Set.diff
                                (Flow.nodes newFlow |> List.map Tuple.first |> Set.fromList)
                                (Flow.nodes flow |> List.map Tuple.first |> Set.fromList)
                                |> Expect.equalSets (Set.singleton addedNode)
            ]
        , Test.describe "isCondition = True" <|
            let
                addParallelAt path rootFlow =
                    Flow.Operation.applyAt
                        opCfg
                        path
                        (AddParallelBelow { isCondition = True } addedNode)
                        rootFlow
            in
            [ Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Always returns True" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            addParallelAt path flow
                                |> Expect.notEqual Nothing
            , Test.test ">0 -> {0, 99?([],[])}" <|
                \() ->
                    addParallelAt [] (Node 0)
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Node 0, Condition addedNode { trueSeq = [], falseSeq = [] } ], [] ))
            , Test.test ">[0,1] -> {[0,1],99?([],[])}" <|
                \() ->
                    addParallelAt [] (Sequential [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Sequential [ Node 0, Node 1 ], Condition addedNode { trueSeq = [], falseSeq = [] } ], [] ))
            , Test.test ">{0,1} ~> {0,1,99?([],[])}" <|
                \() ->
                    addParallelAt [] (Parallel [ Node 0, Node 1 ])
                        |> expectEqualAfterOptimizing (Just ( Parallel [ Node 0, Node 1, Condition addedNode { trueSeq = [], falseSeq = [] } ], [] ))
            , Test.test ">0?([1],[2]) -> {0?([1],[2]), 99?([],[])}" <|
                \() ->
                    addParallelAt [] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Parallel
                                    [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] }
                                    , Condition addedNode { trueSeq = [], falseSeq = [] }
                                    ]
                                , []
                                )
                            )
            , Test.test ">0?([1],[]) -> {0?([1],[]), 99?([],[])}" <|
                \() ->
                    addParallelAt [] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Parallel
                                    [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [] }
                                    , Condition addedNode { trueSeq = [], falseSeq = [] }
                                    ]
                                , []
                                )
                            )
            , Test.test ">0?([],[1]) -> {0?([],[1]), 99?([],[])}" <|
                \() ->
                    addParallelAt [] (Condition 0 { trueSeq = [], falseSeq = [ Node 1 ] })
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Parallel
                                    [ Condition 0 { trueSeq = [], falseSeq = [ Node 1 ] }
                                    , Condition addedNode { trueSeq = [], falseSeq = [] }
                                    ]
                                , []
                                )
                            )
            , Test.test "{>0,1,2} ~> {0,99?([],[]),1,2}" <|
                \() ->
                    addParallelAt [ InParallel 0 ] (Parallel [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Parallel [ Node 0, Condition addedNode { trueSeq = [], falseSeq = [] }, Node 1, Node 2 ]
                                , []
                                )
                            )
            , Test.test "[>0,1,2] ~> [{0,99?([],[])},1,2]" <|
                \() ->
                    addParallelAt [ InSequential 0 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Condition addedNode { trueSeq = [], falseSeq = [] } ], Node 1, Node 2 ]
                                , [ { task = id 1
                                    , newDeps =
                                        [ { branch = 0, id = id addedNode }
                                        , { branch = 1, id = id addedNode }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "{0,>1,2} ~> {0,1,99?([],[]),2}" <|
                \() ->
                    addParallelAt [ InParallel 1 ] (Parallel [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Parallel [ Node 0, Node 1, Condition addedNode { trueSeq = [], falseSeq = [] }, Node 2 ]
                                , []
                                )
                            )
            , Test.test "[0,>1,2] -> [0,{1,99?([],[])},2]" <|
                \() ->
                    addParallelAt [ InSequential 1 ] (Sequential [ Node 0, Node 1, Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Node 0, Parallel [ Node 1, Condition addedNode { trueSeq = [], falseSeq = [] } ], Node 2 ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 2
                                    , newDeps =
                                        [ { branch = 0, id = id addedNode }
                                        , { branch = 1, id = id addedNode }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[{[0,>1,2],3},4] -> [{[0,{1,99?([],[])},2],3},4]" <|
                \() ->
                    addParallelAt
                        [ InSequential 0, InParallel 0, InSequential 1 ]
                        (Sequential
                            [ Parallel
                                [ Sequential [ Node 0, Node 1, Node 2 ]
                                , Node 3
                                ]
                            , Node 4
                            ]
                        )
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential
                                    [ Parallel
                                        [ Sequential
                                            [ Node 0
                                            , Parallel [ Node 1, Condition addedNode { trueSeq = [], falseSeq = [] } ]
                                            , Node 2
                                            ]
                                        , Node 3
                                        ]
                                    , Node 4
                                    ]
                                , [ { task = id addedNode
                                    , newDeps = [ { branch = 0, id = id 0 } ]
                                    , removeDeps = []
                                    }
                                  , { task = id 2
                                    , newDeps =
                                        [ { branch = 0, id = id addedNode }
                                        , { branch = 1, id = id addedNode }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.test "[>{0,1},2] -> [{0,1,99?([],[])},2]" <|
                \() ->
                    addParallelAt [ InSequential 0 ] (Sequential [ Parallel [ Node 0, Node 1 ], Node 2 ])
                        |> expectEqualAfterOptimizing
                            (Just
                                ( Sequential [ Parallel [ Node 0, Node 1, Condition addedNode { trueSeq = [], falseSeq = [] } ], Node 2 ]
                                , [ { task = id 2
                                    , newDeps =
                                        [ { id = id addedNode, branch = 0 }
                                        , { id = id addedNode, branch = 1 }
                                        ]
                                    , removeDeps = []
                                    }
                                  ]
                                )
                            )
            , Test.fuzz2 Unit.Flow.pathFuzzer Unit.Flow.flowFuzzer "Created Condition always is empty" <|
                \path flow ->
                    case Flow.navigateTo path flow of
                        Nothing ->
                            Expect.pass

                        Just _ ->
                            case addParallelAt path flow of
                                Just ( newFlow, _ ) ->
                                    Flow.find
                                        (\f ->
                                            case f of
                                                Condition node { trueSeq, falseSeq } ->
                                                    if node == addedNode then
                                                        List.isEmpty trueSeq && List.isEmpty falseSeq

                                                    else
                                                        False

                                                _ ->
                                                    False
                                        )
                                        newFlow
                                        |> Expect.notEqual Nothing

                                _ ->
                                    Expect.fail "Should have been Just"
            , Test.fuzz2
                Unit.Flow.pathFuzzer
                flowFuzzerWithoutAddedNodeId
                "Nodes stay the same, except for the added one"
              <|
                \path flow ->
                    case addParallelAt path flow of
                        Nothing ->
                            Expect.pass

                        Just ( newFlow, _ ) ->
                            Set.diff
                                (Flow.nodes newFlow |> List.map Tuple.first |> Set.fromList)
                                (Flow.nodes flow |> List.map Tuple.first |> Set.fromList)
                                |> Expect.equalSets (Set.singleton addedNode)
            ]
        ]


reattachFalseInsideCondition : Test
reattachFalseInsideCondition =
    Test.describe "ReattachFalseInsideCondition" <|
        let
            reattachAt i path rootFlow =
                Flow.Operation.applyAt
                    opCfg
                    path
                    (ReattachFalseInsideCondition { index = i })
                    rootFlow
        in
        [ Test.fuzz3
            (Fuzz.intRange 0 5)
            Unit.Flow.flowFuzzer
            Unit.Flow.pathFuzzer
            "Fails exactly when not on a Condition"
          <|
            \i flow path ->
                case Flow.navigateTo path flow of
                    Just (Condition _ _) ->
                        reattachAt i path flow
                            |> Expect.notEqual Nothing

                    Just _ ->
                        reattachAt i path flow
                            |> Expect.equal Nothing

                    _ ->
                        Expect.pass
        , Test.fuzz2
            (Fuzz.intRange 0 3)
            (Fuzz.listOfLengthBetween 0 3 (Unit.Flow.flowFuzzerOfMaxDepth 2))
            "Condition with empty trueSeq"
          <|
            \i falseSeq ->
                let
                    condition =
                        Condition 1 { trueSeq = [], falseSeq = falseSeq }
                in
                reattachAt i [] condition
                    |> expectEqualAfterOptimizing (Just ( condition, [] ))
        , Test.fuzz2
            (Fuzz.listOfLengthBetween 0 2 (Unit.Flow.flowFuzzerOfMaxDepth 3))
            (Fuzz.listOfLengthBetween 0 2 (Unit.Flow.flowFuzzerOfMaxDepth 3))
            "Reattaching at index 0 moves everything from trueSeq to Sequential after the Condition"
          <|
            \trueSeq falseSeq ->
                let
                    condition =
                        Condition 1 { trueSeq = trueSeq, falseSeq = falseSeq }
                in
                reattachAt 0 [] condition
                    |> {- Clear the DependencyChanges, we're too lazy to write those out in this test.
                          We'll check them in some _unit_ test. Technically we're adding edges
                          { from = leftEdgeNode trueSeq, to = rightEdgeNode falseSeq, branch = 1 }.
                       -}
                       Maybe.map (Tuple.mapSecond (\_ -> []))
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential
                                [ Condition 1 { trueSeq = [], falseSeq = falseSeq }
                                , Sequential trueSeq
                                ]
                            , []
                            )
                        )
        , Test.fuzz2
            (Fuzz.listOfLengthBetween 0 2 (Unit.Flow.flowFuzzerOfMaxDepth 3))
            (Fuzz.listOfLengthBetween 0 2 (Unit.Flow.flowFuzzerOfMaxDepth 3))
            "Reattaching at last index is a no-op"
          <|
            \trueSeq falseSeq ->
                let
                    condition =
                        Condition 1 { trueSeq = trueSeq, falseSeq = falseSeq }
                in
                reattachAt (List.length trueSeq) [] condition
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Condition 1 { trueSeq = trueSeq, falseSeq = falseSeq }
                            , []
                            )
                        )
        , Test.test "Reattaching at index 0 - example" <|
            \() ->
                reattachAt 0
                    []
                    (Condition 0
                        { trueSeq = [ Node 1, Node 2, Node 3 ]
                        , falseSeq = [ Node 4 ]
                        }
                    )
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential
                                [ Condition 0 { trueSeq = [], falseSeq = [ Node 4 ] }
                                , Node 1
                                , Node 2
                                , Node 3
                                ]
                            , [ { task = id 1
                                , newDeps = [ { id = id 4, branch = 0 } ]
                                , removeDeps = []
                                }
                              ]
                            )
                        )
        , Test.test "Reattaching at index 1 - example" <|
            \() ->
                reattachAt 1
                    []
                    (Condition 0
                        { trueSeq = [ Node 1, Node 2, Node 3 ]
                        , falseSeq = [ Node 4 ]
                        }
                    )
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential
                                [ Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 4 ] }
                                , Node 2
                                , Node 3
                                ]
                            , [ { task = id 2
                                , newDeps = [ { id = id 4, branch = 0 } ]
                                , removeDeps = []
                                }
                              ]
                            )
                        )
        , Test.test "Reattaching at last index - example" <|
            \() ->
                let
                    condition =
                        Condition 0
                            { trueSeq = [ Node 1, Node 2, Node 3 ]
                            , falseSeq = [ Node 4 ]
                            }
                in
                reattachAt 3 [] condition
                    |> expectEqualAfterOptimizing (Just ( condition, [] ))
        ]


reattachFalseOutsideCondition : Test
reattachFalseOutsideCondition =
    Test.describe "ReattachFalseOutsideCondition" <|
        let
            reattachAt path index rootFlow =
                Flow.Operation.applyAt
                    opCfg
                    path
                    (ReattachFalseOutsideCondition { index = index })
                    rootFlow
        in
        [ Test.fuzz
            (Fuzz.map4
                (\before trueSeq falseSeq after ->
                    { before = before
                    , trueSeq = trueSeq
                    , falseSeq = falseSeq
                    , after = after
                    }
                )
                -- max depth one less than what we usually test
                (Fuzz.listOfLengthBetween 0 2 (Unit.Flow.flowFuzzerOfMaxDepth 3))
                (Fuzz.listOfLengthBetween 0 2 (Unit.Flow.flowFuzzerOfMaxDepth 3))
                (Fuzz.listOfLengthBetween 0 2 (Unit.Flow.flowFuzzerOfMaxDepth 3))
                (Fuzz.listOfLengthBetween 0 2 (Unit.Flow.flowFuzzerOfMaxDepth 3))
            )
            "Condition in sequential"
          <|
            \{ before, trueSeq, falseSeq, after } ->
                let
                    beforeLen =
                        List.length before
                in
                Sequential
                    (before ++ [ Condition 1 { trueSeq = trueSeq, falseSeq = falseSeq } ] ++ after)
                    |> reattachAt [ InSequential beforeLen ] (beforeLen + 2)
                    |> Maybe.map (Tuple.mapSecond (\_ -> []))
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential
                                (before
                                    ++ [ Condition 1
                                            { trueSeq = trueSeq ++ List.take 2 after
                                            , falseSeq = falseSeq
                                            }
                                       ]
                                    ++ List.drop 2 after
                                )
                            , []
                            )
                        )
        , Test.test "Nested condition under true" <|
            \_ ->
                Condition 1
                    { trueSeq =
                        [ Node 2
                        , Condition 3
                            { trueSeq = [ Node 4 ]
                            , falseSeq = [ Node 5 ]
                            }
                        , Node 6
                        , Node 7
                        ]
                    , falseSeq = []
                    }
                    |> reattachAt [ InConditionTrue 1 ] 2
                    |> Maybe.map (Tuple.mapSecond (\_ -> []))
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Condition 1
                                { trueSeq =
                                    [ Node 2
                                    , Condition 3
                                        { trueSeq = [ Node 4, Node 6 ]
                                        , falseSeq = [ Node 5 ]
                                        }
                                    , Node 7
                                    ]
                                , falseSeq = []
                                }
                            , []
                            )
                        )
        , Test.test "Nested condition under false" <|
            \_ ->
                Condition 1
                    { trueSeq = []
                    , falseSeq =
                        [ Node 2
                        , Condition 3
                            { trueSeq = [ Node 4 ]
                            , falseSeq = [ Node 5 ]
                            }
                        , Node 6
                        , Node 7
                        ]
                    }
                    |> reattachAt [ InConditionFalse 1 ] 2
                    |> Maybe.map (Tuple.mapSecond (\_ -> []))
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Condition 1
                                { trueSeq = []
                                , falseSeq =
                                    [ Node 2
                                    , Condition 3
                                        { trueSeq = [ Node 4, Node 6 ]
                                        , falseSeq = [ Node 5 ]
                                        }
                                    , Node 7
                                    ]
                                }
                            , []
                            )
                        )
        ]


remove : Test
remove =
    let
        removeAt path rootFlow =
            Flow.Operation.applyAt
                opCfg
                path
                Remove
                rootFlow
    in
    Test.describe "Remove"
        [ Test.test ">0 -> []" <|
            \() ->
                removeAt [] (Node 0)
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential []
                            , []
                            )
                        )
        , Test.test "[>0] -> []" <|
            \() ->
                removeAt [ InSequential 0 ] (Sequential [ Node 0 ])
                    |> expectEqualAfterOptimizing (Just ( Sequential [], [] ))
        , Test.test ">[0,1] -> []" <|
            \() ->
                removeAt [] (Sequential [ Node 0, Node 1 ])
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential []
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test ">[0,1,2] -> []" <|
            \() ->
                removeAt [] (Sequential [ Node 0, Node 1, Node 2 ])
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential []
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              , { task = id 2
                                , newDeps = []
                                , removeDeps = [ { id = id 1, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test "[>0,1,2] -> [1,2]" <|
            \() ->
                removeAt [ InSequential 0 ] (Sequential [ Node 0, Node 1, Node 2 ])
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential [ Node 1, Node 2 ]
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test "[0,>1,2] -> [0,2]" <|
            \() ->
                removeAt [ InSequential 1 ] (Sequential [ Node 0, Node 1, Node 2 ])
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential [ Node 0, Node 2 ]
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              , { task = id 2
                                , newDeps = [ { id = id 0, branch = 0 } ]
                                , removeDeps = [ { id = id 1, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test ">{0,1} -> []" <|
            \() ->
                removeAt [] (Parallel [ Node 0, Node 1 ])
                    |> expectEqualAfterOptimizing (Just ( Sequential [], [] ))
        , Test.test "[>0,{1,2}] ~> {1,2}" <|
            \() ->
                removeAt [ InSequential 0 ] (Sequential [ Node 0, Parallel [ Node 1, Node 2 ] ])
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Parallel [ Node 1, Node 2 ]
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              , { task = id 2
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test "[0,{>1,2}] ~> [0,2]" <|
            \() ->
                removeAt [ InSequential 1, InParallel 0 ] (Sequential [ Node 0, Parallel [ Node 1, Node 2 ] ])
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential [ Node 0, Node 2 ]
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test "{>0,1} ~> 1" <|
            \() ->
                removeAt [ InParallel 0 ] (Parallel [ Node 0, Node 1 ])
                    |> expectEqualAfterOptimizing (Just ( Node 1, [] ))
        , Test.test "{0,>1,2} -> {0,2}" <|
            \() ->
                removeAt [ InParallel 1 ] (Parallel [ Node 0, Node 1, Node 2 ])
                    |> expectEqualAfterOptimizing (Just ( Parallel [ Node 0, Node 2 ], [] ))
        , Test.test "{0,[>1,2]} ~> {0,2}" <|
            \() ->
                removeAt [ InParallel 1, InSequential 0 ] (Parallel [ Node 0, Sequential [ Node 1, Node 2 ] ])
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Parallel [ Node 0, Node 2 ]
                            , [ { task = id 2
                                , newDeps = []
                                , removeDeps = [ { id = id 1, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test ">0?([1],[2]) ~> {1,2}" <|
            \() ->
                removeAt [] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Parallel
                                [ Node 1
                                , Node 2
                                ]
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              , { task = id 2
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 1 } ]
                                }
                              ]
                            )
                        )
        , Test.test ">0?([1,2],[3,4]) ~> {[1, 2],[3,4]}" <|
            \() ->
                removeAt []
                    (Condition 0
                        { trueSeq = [ Node 1, Node 2 ]
                        , falseSeq = [ Node 3, Node 4 ]
                        }
                    )
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Parallel
                                [ Sequential [ Node 1, Node 2 ]
                                , Sequential [ Node 3, Node 4 ]
                                ]
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              , { task = id 3
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 1 } ]
                                }
                              ]
                            )
                        )
        , Test.test "[0,>1?([2],[3])] ~> [0,{2,3}]" <|
            \() ->
                removeAt [ InSequential 1 ]
                    (Sequential
                        [ Node 0
                        , Condition 1 { trueSeq = [ Node 2 ], falseSeq = [ Node 3 ] }
                        ]
                    )
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential
                                [ Node 0
                                , Parallel
                                    [ Node 2
                                    , Node 3
                                    ]
                                ]
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              , { task = id 2
                                , newDeps = [ { id = id 0, branch = 0 } ]
                                , removeDeps = [ { id = id 1, branch = 0 } ]
                                }
                              , { task = id 3
                                , newDeps = [ { id = id 0, branch = 0 } ]
                                , removeDeps = [ { id = id 1, branch = 1 } ]
                                }
                              ]
                            )
                        )
        , Test.test "0?([>1],[2]) -> 0?([],[2])" <|
            \() ->
                removeAt [ InConditionTrue 0 ] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Condition 0 { trueSeq = [], falseSeq = [ Node 2 ] }
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test "0?([1],[>2]) -> 0?([1],[])" <|
            \() ->
                removeAt [ InConditionFalse 0 ] (Condition 0 { trueSeq = [ Node 1 ], falseSeq = [ Node 2 ] })
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Condition 0 { trueSeq = [ Node 1 ], falseSeq = [] }
                            , [ { task = id 2
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 1 } ]
                                }
                              ]
                            )
                        )
        , Test.test "[0?([>1],[2]),3] -> [0?([],[2]),3]" <|
            \() ->
                removeAt [ InSequential 0, InConditionTrue 0 ]
                    (Sequential
                        [ Condition 0
                            { trueSeq = [ Node 1 ]
                            , falseSeq = [ Node 2 ]
                            }
                        , Node 3
                        ]
                    )
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential
                                [ Condition 0
                                    { trueSeq = []
                                    , falseSeq = [ Node 2 ]
                                    }
                                , Node 3
                                ]
                            , [ { task = id 1
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 0 } ]
                                }
                              , { task = id 3
                                , newDeps = [ { id = id 0, branch = 0 } ]
                                , removeDeps = [ { id = id 1, branch = 0 } ]
                                }
                              ]
                            )
                        )
        , Test.test "[0?([1],[>2]),3] -> [0?([1],[]),3]" <|
            \() ->
                removeAt [ InSequential 0, InConditionFalse 0 ]
                    (Sequential
                        [ Condition 0
                            { trueSeq = [ Node 1 ]
                            , falseSeq = [ Node 2 ]
                            }
                        , Node 3
                        ]
                    )
                    |> expectEqualAfterOptimizing
                        (Just
                            ( Sequential
                                [ Condition 0
                                    { trueSeq = [ Node 1 ]
                                    , falseSeq = []
                                    }
                                , Node 3
                                ]
                            , [ { task = id 2
                                , newDeps = []
                                , removeDeps = [ { id = id 0, branch = 1 } ]
                                }
                              , { task = id 3
                                , newDeps = [ { id = id 0, branch = 1 } ]
                                , removeDeps = [ { id = id 2, branch = 0 } ]
                                }
                              ]
                            )
                        )
        ]



-- FUZZERS


opFuzzer : Fuzzer node -> Fuzzer (Operation node)
opFuzzer nodeFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 (\isCondition node -> AddBefore { isCondition = isCondition } node) Fuzz.bool nodeFuzzer
        , Fuzz.map2 (\isCondition node -> AddAfter { isCondition = isCondition } node) Fuzz.bool nodeFuzzer
        , Fuzz.map2 (\isCondition node -> AddParallelBelow { isCondition = isCondition } node) Fuzz.bool nodeFuzzer
        , Fuzz.map (\i -> ReattachFalseInsideCondition { index = i }) (Fuzz.intRange 0 5)
        , Fuzz.map (\i -> ReattachFalseOutsideCondition { index = i }) (Fuzz.intRange 0 5)
        , Fuzz.constant Remove
        ]

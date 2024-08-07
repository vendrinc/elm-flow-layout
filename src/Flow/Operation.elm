module Flow.Operation exposing
    ( Operation(..), applyAt
    , Config
    )

{-|

@docs Operation, applyAt
@docs Config

-}

import Flow exposing (DependencyChange, Flow(..), Step(..))
import List.Extra


{-| Allowed positions:

  - AddBefore: anywhere
  - AddAfter: anywhere
  - AddParallelBelow: anywhere
  - ReattachFalseInsideCondition: on Condition
  - ReattachFalseOutsideCondition: on Condition
  - Remove: anywhere

-}
type Operation node
    = AddBefore { isCondition : Bool } node
    | AddAfter { isCondition : Bool } node
    | AddParallelBelow { isCondition : Bool } node
    | ReattachFalseInsideCondition { index : Int }
    | ReattachFalseOutsideCondition { index : Int }
    | Remove


{-| -}
type alias Config node =
    { toId : node -> String
    }


{-| `applyAt` assumes you'll later run `optimize` to flatten out nested
Sequentials and Parallels.

TODO: when we see the usage in some kind of `update` function, decide
whether we should call `optimize` here implicitly and remove the above
comment, or whether to let the user decide when to optimize.

-}
applyAt : Config node -> List Step -> Operation node -> Flow node -> Maybe ( Flow node, List DependencyChange )
applyAt cfg steps op rootFlow =
    let
        newRootFlow : Maybe (Flow node)
        newRootFlow =
            case Flow.navigateTo steps rootFlow of
                Just targetFlow ->
                    case op of
                        AddBefore { isCondition } n ->
                            if isCondition then
                                Just <| addConditionBefore steps rootFlow n targetFlow

                            else
                                Just <| addNodeBefore steps rootFlow n

                        AddAfter { isCondition } n ->
                            if isCondition then
                                Just <| addConditionAfter steps rootFlow n targetFlow

                            else
                                Just <| addNodeAfter steps rootFlow n

                        AddParallelBelow { isCondition } n ->
                            if isCondition then
                                Just <| addConditionParallelBelow steps rootFlow n targetFlow

                            else
                                Just <| addNodeParallelBelow steps rootFlow n

                        ReattachFalseInsideCondition { index } ->
                            reattachFalseInsideCondition steps rootFlow index targetFlow

                        ReattachFalseOutsideCondition { index } ->
                            reattachFalseOutsideCondition steps rootFlow index targetFlow

                        Remove ->
                            remove steps rootFlow targetFlow

                Nothing ->
                    {- There is an edge case with eg. InConditionTrue 0 where
                       the condition is empty.

                       Since there is no Flow node we could put into the `addNodeBefore`
                       etc. functions, we'll need another set of functions here that just
                       add to empty list.
                    -}
                    let
                        nodeToAdd : Maybe ( { isCondition : Bool }, node )
                        nodeToAdd =
                            case op of
                                AddBefore c n ->
                                    Just ( c, n )

                                AddAfter c n ->
                                    Just ( c, n )

                                AddParallelBelow c n ->
                                    Just ( c, n )

                                ReattachFalseInsideCondition _ ->
                                    Nothing

                                ReattachFalseOutsideCondition _ ->
                                    Nothing

                                Remove ->
                                    Nothing
                    in
                    case nodeToAdd of
                        Nothing ->
                            Nothing

                        Just ( isCondition, node ) ->
                            case List.reverse steps of
                                [] ->
                                    Nothing

                                (InConditionTrue 0) :: revRest ->
                                    let
                                        parentPath =
                                            List.reverse revRest
                                    in
                                    case Flow.navigateTo parentPath rootFlow of
                                        Just (Condition cNode cBranches) ->
                                            addToEmptyConditionTrue cNode cBranches isCondition node
                                                |> Maybe.map (\newFlow -> Flow.mapAt parentPath (\_ -> newFlow) rootFlow)

                                        _ ->
                                            Nothing

                                (InConditionTrue _) :: _ ->
                                    Nothing

                                (InConditionFalse 0) :: revRest ->
                                    let
                                        parentPath =
                                            List.reverse revRest
                                    in
                                    case Flow.navigateTo parentPath rootFlow of
                                        Just (Condition cNode cBranches) ->
                                            addToEmptyConditionFalse cNode cBranches isCondition node
                                                |> Maybe.map (\newFlow -> Flow.mapAt parentPath (\_ -> newFlow) rootFlow)

                                        _ ->
                                            Nothing

                                (InConditionFalse _) :: _ ->
                                    Nothing

                                (InSequential 0) :: revRest ->
                                    let
                                        parentPath =
                                            List.reverse revRest
                                    in
                                    case Flow.navigateTo parentPath rootFlow of
                                        Just (Sequential xs) ->
                                            addToEmptySequential xs isCondition node
                                                |> Maybe.map (\newFlow -> Flow.mapAt parentPath (\_ -> newFlow) rootFlow)

                                        _ ->
                                            Nothing

                                (InSequential _) :: _ ->
                                    Nothing

                                (InParallel 0) :: revRest ->
                                    let
                                        parentPath =
                                            List.reverse revRest
                                    in
                                    case Flow.navigateTo parentPath rootFlow of
                                        Just (Parallel xs) ->
                                            addToEmptyParallel xs isCondition node
                                                |> Maybe.map (\newFlow -> Flow.mapAt parentPath (\_ -> newFlow) rootFlow)

                                        _ ->
                                            Nothing

                                (InParallel _) :: _ ->
                                    Nothing
    in
    newRootFlow
        |> Maybe.map
            (\newRootFlow_ ->
                ( newRootFlow_
                , Flow.diff
                    { before = rootFlow
                    , after = newRootFlow_
                    , nodeToId = cfg.toId
                    }
                )
            )


addNodeBefore : List Step -> Flow node -> node -> Flow node
addNodeBefore steps rootFlow newNode =
    rootFlow
        |> Flow.mapAt steps (\flow -> Sequential [ Node newNode, flow ])


addNodeAfter : List Step -> Flow node -> node -> Flow node
addNodeAfter steps rootFlow newNode =
    rootFlow
        |> Flow.mapAt steps (\flow -> Sequential [ flow, Node newNode ])


addNodeParallelBelow : List Step -> Flow node -> node -> Flow node
addNodeParallelBelow steps rootFlow newNode =
    rootFlow
        |> Flow.mapAt steps (\flow -> Parallel [ flow, Node newNode ])


addConditionBefore : List Step -> Flow node -> node -> Flow node -> Flow node
addConditionBefore steps rootFlow newNode targetFlow =
    {- We need to start one level below the wanted Condition:
       If it's a Sequential, we want to chomp everything that comes after the Condition (including the targetFlow), into its trueSeq.
       If it's not a Sequential, the trueSeq will only chomp the originally focused node.
    -}
    let
        default () =
            rootFlow
                |> Flow.mapAt steps (\_ -> Condition newNode { trueSeq = [ targetFlow ], falseSeq = [] })
    in
    case List.Extra.unconsLast steps of
        Just ( InSequential n, butLast ) ->
            case Flow.navigateTo butLast rootFlow of
                Just (Sequential xs) ->
                    let
                        ( before, chomped ) =
                            List.Extra.splitAt n xs
                    in
                    rootFlow
                        |> Flow.mapAt butLast
                            (\_ ->
                                Sequential
                                    (before ++ [ Condition newNode { trueSeq = chomped, falseSeq = [] } ])
                            )

                _ ->
                    default ()

        -- These two cases below are a copy-paste of the Sequential above.
        Just ( InConditionTrue n, butLast ) ->
            case Flow.navigateTo butLast rootFlow of
                Just (Condition c { trueSeq, falseSeq }) ->
                    let
                        ( before, chomped ) =
                            List.Extra.splitAt n trueSeq
                    in
                    rootFlow
                        |> Flow.mapAt butLast
                            (\_ ->
                                Condition c
                                    { trueSeq = before ++ [ Condition newNode { trueSeq = chomped, falseSeq = [] } ]
                                    , falseSeq = falseSeq
                                    }
                            )

                _ ->
                    default ()

        Just ( InConditionFalse n, butLast ) ->
            case Flow.navigateTo butLast rootFlow of
                Just (Condition c { trueSeq, falseSeq }) ->
                    let
                        ( before, chomped ) =
                            List.Extra.splitAt n falseSeq
                    in
                    rootFlow
                        |> Flow.mapAt butLast
                            (\_ ->
                                Condition c
                                    { trueSeq = trueSeq
                                    , falseSeq = before ++ [ Condition newNode { trueSeq = chomped, falseSeq = [] } ]
                                    }
                            )

                _ ->
                    default ()

        _ ->
            default ()


addConditionAfter : List Step -> Flow node -> node -> Flow node -> Flow node
addConditionAfter steps rootFlow newNode targetFlow =
    {- We need to start one level below the wanted Condition:
       If it's a Sequential, we want to chomp everything that comes after the Condition (thus excluding the targetFlow), into its trueSeq.
       If it's not a Sequential, we add an empty Condition after the originally focused node.
    -}
    let
        default () =
            rootFlow
                |> Flow.mapAt steps
                    (\_ ->
                        Sequential
                            [ targetFlow
                            , Condition newNode { trueSeq = [], falseSeq = [] }
                            ]
                    )
    in
    case List.Extra.unconsLast steps of
        Just ( InSequential n, butLast ) ->
            case Flow.navigateTo butLast rootFlow of
                Just (Sequential xs) ->
                    let
                        ( before, chomped ) =
                            -- this n+1 is the "after" (as opposed to n == "before")
                            List.Extra.splitAt (n + 1) xs
                    in
                    rootFlow
                        |> Flow.mapAt butLast
                            (\_ ->
                                Sequential
                                    (before ++ [ Condition newNode { trueSeq = chomped, falseSeq = [] } ])
                            )

                _ ->
                    default ()

        _ ->
            default ()


addConditionParallelBelow : List Step -> Flow node -> node -> Flow node -> Flow node
addConditionParallelBelow steps rootFlow newNode targetFlow =
    -- With Parallel, we don't do any chomping of nodes into the Condition. The condition we generate is always empty.
    rootFlow
        |> Flow.mapAt steps
            (\_ ->
                Parallel
                    [ targetFlow
                    , Condition newNode { trueSeq = [], falseSeq = [] }
                    ]
            )


reattachFalseInsideCondition : List Step -> Flow node -> Int -> Flow node -> Maybe (Flow node)
reattachFalseInsideCondition steps rootFlow index targetFlow =
    case targetFlow of
        Condition n { trueSeq, falseSeq } ->
            let
                ( newTrueSeq, addedAfter ) =
                    List.Extra.splitAt index trueSeq
            in
            if List.isEmpty addedAfter then
                Just rootFlow

            else
                rootFlow
                    |> Flow.mapAt steps
                        (\_ ->
                            Sequential
                                [ Condition n { trueSeq = newTrueSeq, falseSeq = falseSeq }
                                , Sequential addedAfter
                                ]
                        )
                    |> Just

        _ ->
            Nothing


reattachFalseOutsideCondition : List Step -> Flow node -> Int -> Flow node -> Maybe (Flow node)
reattachFalseOutsideCondition steps rootFlow index targetFlow =
    case targetFlow of
        Condition condNode { trueSeq, falseSeq } ->
            let
                updateSeq seqIndex seq =
                    let
                        ( beforeCond, condAndAfter ) =
                            List.Extra.splitAt seqIndex seq

                        ( newTrueEnd, newAfterCond ) =
                            List.Extra.splitAt (index - seqIndex + 1) condAndAfter

                        newCond =
                            Condition condNode
                                { trueSeq = trueSeq ++ List.drop 1 newTrueEnd
                                , falseSeq = falseSeq
                                }
                    in
                    beforeCond ++ [ newCond ] ++ newAfterCond

                revSteps =
                    List.reverse steps
            in
            Flow.mapAt (List.reverse (List.drop 1 revSteps))
                (\flow ->
                    case ( revSteps, flow ) of
                        ( (InSequential seqIndex) :: _, Sequential parentSeq ) ->
                            Sequential (updateSeq seqIndex parentSeq)

                        ( (InConditionTrue seqindex) :: _, Condition parentCond parentSeqs ) ->
                            Condition parentCond
                                { parentSeqs
                                    | trueSeq = updateSeq seqindex parentSeqs.trueSeq
                                }

                        ( (InConditionFalse seqindex) :: _, Condition parentCond parentSeqs ) ->
                            Condition parentCond
                                { parentSeqs
                                    | falseSeq = updateSeq seqindex parentSeqs.falseSeq
                                }

                        _ ->
                            flow
                )
                rootFlow
                |> Just

        _ ->
            Nothing


remove : List Step -> Flow node -> Flow node -> Maybe (Flow node)
remove steps rootFlow targetFlow =
    case targetFlow of
        Condition _ seqs ->
            Just <|
                Flow.mapAt steps
                    (\_ ->
                        Parallel
                            [ Sequential seqs.trueSeq
                            , Sequential seqs.falseSeq
                            ]
                    )
                    rootFlow

        _ ->
            Just <| Flow.mapAt steps (\_ -> Sequential []) rootFlow


{-| Note the condition will be empty - no gobbling up the rest of the sequential after it.
Thus this is only helpful for the "add into empty list" edge cases, not for `addBefore` / `addAfter` in general.
-}
flowFromNode : { isCondition : Bool } -> node -> Flow node
flowFromNode { isCondition } node =
    if isCondition then
        Condition node { trueSeq = [], falseSeq = [] }

    else
        Node node


addToEmptyConditionTrue : node -> { trueSeq : List (Flow node), falseSeq : List (Flow node) } -> { isCondition : Bool } -> node -> Maybe (Flow node)
addToEmptyConditionTrue conditionNode { trueSeq, falseSeq } isCondition nodeToAdd =
    if List.isEmpty trueSeq then
        Just
            (Condition conditionNode
                { trueSeq = [ flowFromNode isCondition nodeToAdd ]
                , falseSeq = falseSeq
                }
            )

    else
        Nothing


addToEmptyConditionFalse : node -> { trueSeq : List (Flow node), falseSeq : List (Flow node) } -> { isCondition : Bool } -> node -> Maybe (Flow node)
addToEmptyConditionFalse conditionNode { trueSeq, falseSeq } isCondition nodeToAdd =
    if List.isEmpty falseSeq then
        Just
            (Condition conditionNode
                { trueSeq = trueSeq
                , falseSeq = [ flowFromNode isCondition nodeToAdd ]
                }
            )

    else
        Nothing


addToEmptySequential : List (Flow node) -> { isCondition : Bool } -> node -> Maybe (Flow node)
addToEmptySequential xs isCondition nodeToAdd =
    if List.isEmpty xs then
        Just (Sequential [ flowFromNode isCondition nodeToAdd ])

    else
        Nothing


addToEmptyParallel : List (Flow node) -> { isCondition : Bool } -> node -> Maybe (Flow node)
addToEmptyParallel xs isCondition nodeToAdd =
    if List.isEmpty xs then
        Just (Parallel [ flowFromNode isCondition nodeToAdd ])

    else
        Nothing

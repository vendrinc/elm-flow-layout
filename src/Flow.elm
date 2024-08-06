module Flow exposing
    ( Flow(..)
    , getNode
    , isSequential, isParallel, isCondition, isEmpty
    , descendants, nodes
    , leftEdge, rightEdgeNodes, dependencies, dependants
    , findNode
    , edges
    , toDot
    , DependencyChange, diff
    , breadthFirstFold
    , findMap
    , optimize, sortParallel
    , map, mapWithNode, filter
    , Step(..), navigateTo, mapAt, previousPath, nextPath, find
    )

{-|


# Workflow

@docs Flow


# Query

@docs getNode
@docs isSequential, isParallel, isCondition, isEmpty
@docs descendants, nodes
@docs leftEdge, rightEdgeNodes, dependencies, dependants
@docs findNode
@docs edges
@docs toDot
@docs DependencyChange, diff
@docs breadthFirstFold
@docs findMap


# Update

@docs optimize, sortParallel
@docs map, mapWithNode, filter
@docs Step, navigateTo, mapAt, previousPath, nextPath, find

-}

import AssocSet
import Dict
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Transform
import Util.Dot


{-| Note that `Sequential []` is our designated "empty" node.
`Parallel []` could fit the same purpose technically but we prefer Sequential.
We coerce `Parallel []` into `Sequential []` when running `optimize`.

DESIGN NOTE: Another option would be to have non-empty lists in Sequential and
Parallel and have an explicit `Empty` constructor.

-}
type Flow node
    = Node node
    | Sequential (List (Flow node))
    | Parallel (List (Flow node))
    | Condition
        node
        -- The branches of Condition are automatically Sequential.
        -- This is important for functions like `reattachFalse`.
        { trueSeq : List (Flow node)
        , falseSeq : List (Flow node)
        }


getNode : Flow node -> Maybe node
getNode flow =
    case flow of
        Node n ->
            Just n

        Condition n _ ->
            Just n

        Sequential _ ->
            Nothing

        Parallel _ ->
            Nothing


isSequential : Flow node -> Bool
isSequential flow =
    case flow of
        Sequential _ ->
            True

        _ ->
            False


isParallel : Flow node -> Bool
isParallel flow =
    case flow of
        Parallel _ ->
            True

        _ ->
            False


isCondition : Flow node -> Bool
isCondition flow =
    case flow of
        Condition _ _ ->
            True

        _ ->
            False


{-| Apply a function to every node.
-}
map : (a -> b) -> Flow a -> Flow b
map fn flow =
    case flow of
        Node node ->
            Node (fn node)

        Sequential items ->
            items
                |> List.map (map fn)
                |> Sequential

        Parallel items ->
            items
                |> List.map (map fn)
                |> Parallel

        Condition node { trueSeq, falseSeq } ->
            Condition (fn node)
                { trueSeq = List.map (map fn) trueSeq
                , falseSeq = List.map (map fn) falseSeq
                }


{-| Apply a function to every node, having acces to the Flow value itself.
-}
mapWithNode : (Flow a -> a -> b) -> Flow a -> Flow b
mapWithNode fn flow =
    case flow of
        Node node ->
            Node (fn flow node)

        Sequential items ->
            items
                |> List.map (\item -> map (fn item) item)
                |> Sequential

        Parallel items ->
            items
                |> List.map (\item -> map (fn item) item)
                |> Parallel

        Condition node { trueSeq, falseSeq } ->
            Condition (fn flow node)
                { trueSeq = List.map (\item -> map (fn item) item) trueSeq
                , falseSeq = List.map (\item -> map (fn item) item) falseSeq
                }


{-| Keeping nodes satisfying a predicate
-}
filter : (a -> Bool) -> Flow a -> Flow a
filter pred flow =
    case flow of
        Node node ->
            if pred node then
                Node node

            else
                Sequential []

        Sequential items ->
            Sequential (List.map (filter pred) items)

        Parallel items ->
            Parallel (List.map (filter pred) items)

        Condition node { trueSeq, falseSeq } ->
            if pred node then
                Condition node
                    { trueSeq = List.map (filter pred) trueSeq
                    , falseSeq = List.map (filter pred) falseSeq
                    }

            else
                Sequential []


findNode : (a -> Maybe b) -> Flow a -> Maybe b
findNode fn flow =
    case flow of
        Node node ->
            fn node

        Sequential seq ->
            List.Extra.findMap (findNode fn) seq

        Parallel par ->
            List.Extra.findMap (findNode fn) par

        Condition node { trueSeq, falseSeq } ->
            fn node
                |> Maybe.Extra.orElseLazy (\_ -> List.Extra.findMap (findNode fn) trueSeq)
                |> Maybe.Extra.orElseLazy (\_ -> List.Extra.findMap (findNode fn) falseSeq)


{-| OPTIMIZE: apply simplifications until you reach a fixpoint.
This uses a library Janiczek/transform that handles that fixpoint boilerplate.
-}
optimize : Flow node -> Flow node
optimize flow =
    Transform.transformAll
        recurse
        optimizeAll
        flow


{-| Returns all recursive children of the Flow (including the input Flow itself),
in a depth-first traversal.

    Sequential
        [ Node 1
        , Parallel
            [ Node 2
            , Node 3
            ]
        , Condition 4
            { trueSeq = [ Node 5 ]
            , falseSeq = [ Node 6 ]
            }
        ]
        |> descendants
        == [ Sequential [ Node 1, Parallel [ Node 2, Node 3 ], Condition 4 { trueSeq = [ Node 5 ], falseSeq = [ Node 6 ] } ]
           , Node 1
           , Parallel [ Node 2, Node 3 ]
           , Node 2
           , Node 3
           , Condition 4 { trueSeq = [ Node 5 ], falseSeq = [ Node 6 ] }
           , Node 5
           , Node 6
           ]

-}
descendants : Flow node -> List (Flow node)
descendants flow =
    Transform.children
        recursiveChildren
        flow


nodes : Flow node -> List ( node, List Step )
nodes rootFlow =
    -- TODO can `nodes` and `find` be expressed via some `fold`?
    let
        go : List ( Flow node, List Step ) -> List ( node, List Step ) -> List ( node, List Step )
        go todos acc =
            case todos of
                [] ->
                    List.reverse acc

                ( todoFlow, todoPath ) :: rest ->
                    case todoFlow of
                        Node n ->
                            go
                                rest
                                (( n, List.reverse todoPath ) :: acc)

                        Sequential xs ->
                            go
                                (List.indexedMap (\i x -> ( x, InSequential i :: todoPath )) xs ++ rest)
                                acc

                        Parallel xs ->
                            go
                                (List.indexedMap (\i x -> ( x, InParallel i :: todoPath )) xs ++ rest)
                                acc

                        Condition n { trueSeq, falseSeq } ->
                            go
                                (List.indexedMap (\i x -> ( x, InConditionTrue i :: todoPath )) trueSeq
                                    ++ List.indexedMap (\i x -> ( x, InConditionFalse i :: todoPath )) falseSeq
                                    ++ rest
                                )
                                (( n, List.reverse todoPath ) :: acc)
    in
    go [ ( rootFlow, [] ) ] []


{-| Fold by visiting nodes in the same order they would get unblocked.

    0 ──╮── 1 ─── 3 ── 6 ──╭─ 3
        ╰── 2 ─╮─ 4 ──╭────╯
               ╰─ 5 ──╯

Unlike parallel tracks, condition branches are visited entirely before the next one.

               ᴛʀᴜᴇ
    0 ──╮──╮── 1 ─── 3 ── 6 ──╭──╭─── 10
        │  ╰── 2 ─╮─ 4 ─╭─────╯  │
        │         ╰─ 5 ─╯        │
        │      ғᴀʟsᴇ             │
        ╰──╮── 7 ─── 9 ─╭────────╯
           ╰── 8 ───────╯

-}
breadthFirstFold : (a -> b -> b) -> b -> Flow a -> b
breadthFirstFold fn initState rootFlow =
    let
        next : Flow a -> b -> ( b, List (Flow a) )
        next flow state =
            case flow of
                Node node ->
                    ( fn node state, [] )

                Sequential seq ->
                    case splitOnNonEmpty seq of
                        Just ( head, rest ) ->
                            next head state
                                |> Tuple.mapSecond (\after -> wrapIfMultiple Sequential (after ++ rest))

                        Nothing ->
                            ( state, [] )

                Parallel par ->
                    par
                        |> List.foldl nextParallel ( state, [] )
                        |> Tuple.mapSecond (wrapIfMultiple Parallel)

                Condition node { trueSeq, falseSeq } ->
                    ( fn node state
                        |> foldBranchSeq trueSeq
                        |> foldBranchSeq falseSeq
                    , []
                    )

        wrapIfMultiple : (List (Flow a) -> Flow a) -> List (Flow a) -> List (Flow a)
        wrapIfMultiple wrap items =
            case items of
                (_ :: _ :: _) as xs ->
                    [ wrap xs ]

                xs ->
                    xs

        nextParallel : Flow a -> ( b, List (Flow a) ) -> ( b, List (Flow a) )
        nextParallel flow ( state, after ) =
            next flow state
                |> Tuple.mapSecond (\newAfter -> after ++ newAfter)

        foldBranchSeq : List (Flow a) -> b -> b
        foldBranchSeq seq state =
            List.foldl (\f s -> breadthFirstFold fn s f) state seq

        go : Flow a -> b -> b
        go flow state =
            case next flow state of
                ( newState, [] ) ->
                    newState

                ( newState, after ) ->
                    List.foldl go newState after
    in
    go rootFlow initState


splitOnNonEmpty : List (Flow node) -> Maybe ( Flow node, List (Flow node) )
splitOnNonEmpty flows =
    case flows of
        [] ->
            Nothing

        flow :: rest ->
            if isEmpty flow then
                splitOnNonEmpty rest

            else
                Just ( flow, rest )


leftEdge : Flow node -> List { path : List Step, node : node }
leftEdge flow =
    case flow of
        Node n ->
            [ { path = [], node = n } ]

        Condition n _ ->
            [ { path = [], node = n } ]

        Sequential xs ->
            leftEdgeSeq 0 xs

        Parallel xs ->
            xs
                |> List.indexedMap
                    (\i x ->
                        leftEdge x
                            |> List.map (\item -> { item | path = InParallel i :: item.path })
                    )
                |> List.concat


leftEdgeSeq : Int -> List (Flow node) -> List { path : List Step, node : node }
leftEdgeSeq index items =
    case items of
        [] ->
            []

        x :: rest ->
            if isEmpty x then
                leftEdgeSeq (index + 1) rest

            else
                leftEdge x
                    |> List.map (\item -> { item | path = InSequential index :: item.path })


dependencies : List Step -> Flow node -> List { node : node, branch : Int }
dependencies steps rootFlow =
    let
        b0 node =
            { node = node, branch = 0 }

        b1 node =
            { node = node, branch = 1 }
    in
    case navigateTo steps rootFlow of
        Nothing ->
            []

        Just targetFlow ->
            if leftEdge targetFlow == [] then
                []

            else
                case List.reverse steps of
                    [] ->
                        -- if you're the only item, you have no dependencies
                        []

                    (InSequential 0) :: rest ->
                        -- dependencies of 1st item in Sequential are the same as dependencies of the Sequential itself
                        dependencies (List.reverse rest) rootFlow

                    (InSequential n) :: rest ->
                        -- dependencies of 2nd+ item in Sequential are the right edge of the item preceding it
                        navigateTo (List.reverse (InSequential (n - 1) :: rest)) rootFlow
                            |> Maybe.map (rightEdgeNodes >> List.map b0)
                            |> Maybe.withDefault []

                    (InParallel _) :: rest ->
                        -- all parallel rows depend on the item preceding the Parallel
                        previousPath (List.reverse rest)
                            |> Maybe.andThen (\previous -> navigateTo previous rootFlow)
                            |> Maybe.map (rightEdgeNodes >> List.map b0)
                            |> Maybe.withDefault []

                    (InConditionTrue 0) :: rest ->
                        -- 1st trueSeq item depends on the Condition itself, with branch=0
                        navigateTo (List.reverse rest) rootFlow
                            |> Maybe.andThen getNode
                            |> Maybe.map (b0 >> List.singleton)
                            |> Maybe.withDefault []

                    (InConditionTrue n) :: rest ->
                        -- same as `InSequential n`
                        navigateTo (List.reverse (InConditionTrue (n - 1) :: rest)) rootFlow
                            |> Maybe.map (rightEdgeNodes >> List.map b0)
                            |> Maybe.withDefault []

                    (InConditionFalse 0) :: rest ->
                        -- 1st falseSeq item depends on the Condition itself, with branch=1
                        navigateTo (List.reverse rest) rootFlow
                            |> Maybe.andThen getNode
                            |> Maybe.map (b1 >> List.singleton)
                            |> Maybe.withDefault []

                    (InConditionFalse n) :: rest ->
                        -- same as `InSequential n`
                        navigateTo (List.reverse (InConditionFalse (n - 1) :: rest)) rootFlow
                            |> Maybe.map (rightEdgeNodes >> List.map b0)
                            |> Maybe.withDefault []


dependants : List Step -> Flow node -> List { path : List Step, node : node, branch : Int }
dependants steps rootFlow =
    let
        next path =
            case nextPath path rootFlow of
                Nothing ->
                    []

                Just nextPath_ ->
                    case navigateTo nextPath_ rootFlow of
                        Nothing ->
                            []

                        Just nextFlow ->
                            if isEmpty nextFlow then
                                -- Next thing is an empty sequential/parallel
                                -- The dependant is actually the next non-empty thing
                                next nextPath_

                            else
                                leftEdge nextFlow
                                    |> List.map
                                        (\item ->
                                            { path = nextPath_ ++ item.path
                                            , node = item.node
                                            , branch = 0
                                            }
                                        )
    in
    case navigateTo steps rootFlow of
        Nothing ->
            []

        Just (Condition _ { trueSeq, falseSeq }) ->
            {-
               If both trueSeq and falseSeq are empty, we return two edges, one for each branch (0, 1).

            -}
            if List.all isEmpty trueSeq && List.all isEmpty falseSeq then
                next steps
                    |> List.concatMap
                        (\e ->
                            [ { e | branch = 0 }
                            , { e | branch = 1 }
                            ]
                        )

            else
                let
                    convertSequentialFirstStepTo : (Int -> Step) -> List Step -> List Step
                    convertSequentialFirstStepTo constr steps_ =
                        case steps_ of
                            (InSequential n) :: rest ->
                                constr n :: rest

                            _ ->
                                steps_
                in
                {- Dependants of a Condition are its trueSeq and falseSeq left edges.
                   But if the trueSeq or falseSeq is empty, the thing _after_ the Condition is a dependant instead.
                -}
                List.concat
                    [ if List.all isEmpty trueSeq then
                        next steps
                            |> List.map (\e -> { e | branch = 0 })

                      else
                        leftEdge (Sequential trueSeq)
                            |> List.map
                                (\item ->
                                    { path = steps ++ convertSequentialFirstStepTo InConditionTrue item.path
                                    , node = item.node
                                    , branch = 0
                                    }
                                )
                    , if List.all isEmpty falseSeq then
                        next steps
                            |> List.map (\e -> { e | branch = 1 })

                      else
                        leftEdge (Sequential falseSeq)
                            |> List.map
                                (\item ->
                                    { path = steps ++ convertSequentialFirstStepTo InConditionFalse item.path
                                    , node = item.node
                                    , branch = 1
                                    }
                                )
                    ]

        Just _ ->
            next steps


previousPath : List Step -> Maybe (List Step)
previousPath path =
    let
        finish : List Step -> Maybe (List Step)
        finish steps =
            Just (List.reverse steps)

        outIfFirst : (Int -> Step) -> Int -> List Step -> Maybe (List Step)
        outIfFirst constr n rest =
            if n <= 0 then
                finish rest

            else
                finish (constr (n - 1) :: rest)
    in
    case List.reverse path of
        [] ->
            Nothing

        (InSequential n) :: rest ->
            outIfFirst InSequential n rest

        (InParallel n) :: rest ->
            outIfFirst InParallel n rest

        (InConditionTrue n) :: rest ->
            outIfFirst InConditionTrue n rest

        (InConditionFalse n) :: rest ->
            outIfFirst InConditionFalse n rest


nextPath : List Step -> Flow node -> Maybe (List Step)
nextPath path rootFlow =
    let
        finish : List Step -> Maybe (List Step)
        finish steps =
            Just (List.reverse steps)

        seq : List (Flow node) -> (Int -> Step) -> Int -> List Step -> Maybe (List Step)
        seq xs constr n rest =
            let
                length =
                    List.length xs
            in
            if n == length - 1 then
                -- Can't just go to the next sequential child, we need to go to the sibling of the sequential itself
                nextPath (List.reverse rest) rootFlow

            else
                -- There's another sequential child
                finish (constr (n + 1) :: rest)
    in
    case List.reverse path of
        [] ->
            Nothing

        (InSequential n) :: rest ->
            case navigateTo (List.reverse rest) rootFlow of
                Just (Sequential xs) ->
                    seq xs InSequential n rest

                _ ->
                    Nothing

        (InParallel _) :: rest ->
            -- we need to go to the sibling of the parallel
            nextPath (List.reverse rest) rootFlow

        (InConditionTrue n) :: rest ->
            case navigateTo (List.reverse rest) rootFlow of
                Just (Condition _ { trueSeq }) ->
                    seq trueSeq InConditionTrue n rest

                _ ->
                    Nothing

        (InConditionFalse n) :: rest ->
            case navigateTo (List.reverse rest) rootFlow of
                Just (Condition _ { falseSeq }) ->
                    seq falseSeq InConditionFalse n rest

                _ ->
                    Nothing


isEmpty : Flow node -> Bool
isEmpty flow =
    case flow of
        Node _ ->
            False

        Condition _ _ ->
            False

        Sequential seq ->
            List.all isEmpty seq

        Parallel par ->
            List.all isEmpty par


{-| Collect the right-most nodes.
-}
rightEdgeNodes : Flow node -> List node
rightEdgeNodes flow =
    case flow of
        Node n ->
            [ n ]

        Sequential xs ->
            xs
                |> List.filter (not << isEmpty)
                |> List.Extra.last
                |> Maybe.map rightEdgeNodes
                |> Maybe.withDefault []

        Parallel xs ->
            List.concatMap rightEdgeNodes xs

        Condition n { trueSeq, falseSeq } ->
            let
                trueRightEdgeNodes =
                    rightEdgeNodes (Sequential trueSeq)

                falseRightEdgeNodes =
                    rightEdgeNodes (Sequential falseSeq)
            in
            (if List.isEmpty trueRightEdgeNodes || List.isEmpty falseRightEdgeNodes then
                [ n ]

             else
                []
            )
                ++ trueRightEdgeNodes
                ++ falseRightEdgeNodes


type Step
    = InSequential Int
    | InParallel Int
    | InConditionTrue Int -- steps into its pseudo-Sequential
    | InConditionFalse Int -- steps into its pseudo-Sequential


navigateTo : List Step -> Flow node -> Maybe (Flow node)
navigateTo steps flow =
    case steps of
        [] ->
            Just flow

        step :: rest ->
            case ( step, flow ) of
                ( InSequential index, Sequential xs ) ->
                    case List.Extra.getAt index xs of
                        Nothing ->
                            Nothing

                        Just child ->
                            navigateTo rest child

                ( InSequential _, _ ) ->
                    Nothing

                ( InParallel index, Parallel xs ) ->
                    case List.Extra.getAt index xs of
                        Nothing ->
                            Nothing

                        Just child ->
                            navigateTo rest child

                ( InParallel _, _ ) ->
                    Nothing

                ( InConditionTrue index, Condition _ r ) ->
                    case List.Extra.getAt index r.trueSeq of
                        Nothing ->
                            Nothing

                        Just child ->
                            navigateTo rest child

                ( InConditionTrue _, _ ) ->
                    Nothing

                ( InConditionFalse index, Condition _ r ) ->
                    case List.Extra.getAt index r.falseSeq of
                        Nothing ->
                            Nothing

                        Just child ->
                            navigateTo rest child

                ( InConditionFalse _, _ ) ->
                    Nothing


mapAt : List Step -> (Flow node -> Flow node) -> Flow node -> Flow node
mapAt steps fn flow =
    case steps of
        [] ->
            fn flow

        step :: rest ->
            case ( step, flow ) of
                ( InSequential index, Sequential xs ) ->
                    Sequential (List.Extra.updateAt index (mapAt rest fn) xs)

                ( InSequential _, _ ) ->
                    flow

                ( InParallel index, Parallel xs ) ->
                    Parallel (List.Extra.updateAt index (mapAt rest fn) xs)

                ( InParallel _, _ ) ->
                    flow

                ( InConditionTrue index, Condition n r ) ->
                    Condition n { r | trueSeq = List.Extra.updateAt index (mapAt rest fn) r.trueSeq }

                ( InConditionTrue _, _ ) ->
                    flow

                ( InConditionFalse index, Condition n r ) ->
                    Condition n { r | falseSeq = List.Extra.updateAt index (mapAt rest fn) r.falseSeq }

                ( InConditionFalse _, _ ) ->
                    flow


{-| The `before` and `after` record fields are meant in the workflow sense:
you need to first complete the `before` step,
then you can complete the `after` step.

     edges (Seq [ Node 0, Node 1 ])
     -->
     [ { before = 0, after = 1, branch = 0 } ]

-}
edges : { nodeToId : node -> String } -> Flow node -> List { before : String, after : String, branch : Int }
edges { nodeToId } flow =
    let
        go :
            AssocSet.Set { before : String, after : String, branch : Int }
            -> List { path : List Step, node : node }
            -> List { before : String, after : String, branch : Int }
            -> List { before : String, after : String, branch : Int }
        go seen todos acc =
            case todos of
                [] ->
                    List.reverse acc

                todo :: rest ->
                    let
                        toId : String
                        toId =
                            nodeToId todo.node

                        results : List { path : List Step, node : node, branch : Int }
                        results =
                            dependants todo.path flow

                        addedTodos : List { path : List Step, node : node }
                        addedTodos =
                            results
                                |> List.map (\r -> { path = r.path, node = r.node })

                        newTodos : List { path : List Step, node : node }
                        newTodos =
                            addedTodos ++ rest

                        addedAcc : List { before : String, after : String, branch : Int }
                        addedAcc =
                            results
                                |> List.filterMap
                                    (\r ->
                                        let
                                            wantedEdge =
                                                { before = toId
                                                , after = nodeToId r.node
                                                , branch = r.branch
                                                }
                                        in
                                        if AssocSet.member wantedEdge seen then
                                            Nothing

                                        else
                                            Just wantedEdge
                                    )

                        newAcc : List { before : String, after : String, branch : Int }
                        newAcc =
                            List.reverse addedAcc ++ acc

                        newSeen : AssocSet.Set { before : String, after : String, branch : Int }
                        newSeen =
                            List.foldl
                                AssocSet.insert
                                seen
                                addedAcc
                    in
                    go newSeen newTodos newAcc
    in
    go AssocSet.empty (leftEdge flow) []


toDot : { nodeToId : node -> String } -> Flow node -> String
toDot ({ nodeToId } as cfg) flow =
    let
        conditionNodeIds : Set String
        conditionNodeIds =
            flow
                |> descendants
                |> List.filter isCondition
                |> List.filterMap getNode
                |> List.map nodeToId
                |> Set.fromList

        nodes_ : List Util.Dot.Vertex
        nodes_ =
            nodes flow
                |> List.map
                    (\( node, _ ) ->
                        let
                            id : String
                            id =
                                node |> nodeToId
                        in
                        { id = id
                        , label = id
                        , bgcolor =
                            if Set.member id conditionNodeIds then
                                Just "pink"

                            else
                                Nothing
                        }
                    )

        edges_ : List Util.Dot.Edge
        edges_ =
            edges cfg flow
                |> List.map
                    (\{ before, after, branch } ->
                        -- We want dependencies to point to descendants
                        { from = before
                        , to = after
                        , label = Just (String.fromInt branch)
                        }
                    )
    in
    Util.Dot.toDot nodes_ edges_


type alias DependencyChange =
    { task : String
    , newDeps : List { branch : Int, id : String }
    , removeDeps : List { branch : Int, id : String }
    }


{-| Taken from Dict.Extra.groupBy
-}
groupBy : (a -> String) -> List a -> Dict.Dict String (List a)
groupBy toId list =
    List.foldr
        (\x acc ->
            Dict.update (toId x) (Maybe.map ((::) x) >> Maybe.withDefault [ x ] >> Just) acc
        )
        Dict.empty
        list


diff :
    { before : Flow node
    , after : Flow node
    , nodeToId : node -> String
    }
    -> List DependencyChange
diff { before, after, nodeToId } =
    let
        edgesBefore : Dict.Dict String (List { id : String, branch : Int })
        edgesBefore =
            edges { nodeToId = nodeToId } before
                |> groupBy .after
                |> Dict.map (\_ vs -> List.map (\v -> { id = v.before, branch = v.branch }) vs)

        edgesAfter : Dict.Dict String (List { id : String, branch : Int })
        edgesAfter =
            edges { nodeToId = nodeToId } after
                |> groupBy .after
                |> Dict.map (\_ vs -> List.map (\v -> { id = v.before, branch = v.branch }) vs)

        updateFn from fn =
            Dict.update from
                (Maybe.withDefault { newDeps = [], removeDeps = [] }
                    >> fn
                )
    in
    Dict.merge
        (\from inBefore -> updateFn from (\val -> Just { val | removeDeps = val.removeDeps ++ inBefore }))
        (\from inBefore inAfter ->
            updateFn from
                (\val ->
                    let
                        fromList =
                            List.map (\r -> ( r.id, r.branch )) >> Set.fromList

                        toList =
                            Set.toList >> List.map (\( id, branch ) -> { id = id, branch = branch })

                        inBefore_ : Set ( String, Int )
                        inBefore_ =
                            fromList inBefore

                        inAfter_ : Set ( String, Int )
                        inAfter_ =
                            fromList inAfter

                        added =
                            Set.diff inAfter_ inBefore_

                        removed =
                            Set.diff inBefore_ inAfter_

                        newDeps =
                            val.newDeps ++ toList added

                        removeDeps =
                            val.removeDeps ++ toList removed
                    in
                    if List.isEmpty newDeps && List.isEmpty removeDeps then
                        Nothing

                    else
                        Just
                            { newDeps = newDeps
                            , removeDeps = removeDeps
                            }
                )
        )
        (\from inAfter -> updateFn from (\val -> Just { val | newDeps = val.newDeps ++ inAfter }))
        edgesBefore
        edgesAfter
        Dict.empty
        |> Dict.toList
        |> List.map
            (\( from, r ) ->
                { task = from
                , newDeps = r.newDeps
                , removeDeps = r.removeDeps
                }
            )


findMap : (Flow node -> Maybe a) -> Flow node -> Maybe ( a, List Step )
findMap fn rootFlow =
    -- TODO can `nodes` and `find` be expressed via some `fold`?
    let
        go : List ( Flow node, List Step ) -> Maybe ( a, List Step )
        go todos =
            case todos of
                [] ->
                    Nothing

                ( todoFlow, todoPath ) :: rest ->
                    case fn todoFlow of
                        Just result ->
                            Just ( result, List.reverse todoPath )

                        Nothing ->
                            case todoFlow of
                                Node _ ->
                                    go rest

                                Sequential xs ->
                                    go <| rest ++ List.indexedMap (\i x -> ( x, InSequential i :: todoPath )) xs

                                Parallel xs ->
                                    go <| rest ++ List.indexedMap (\i x -> ( x, InParallel i :: todoPath )) xs

                                Condition _ { trueSeq, falseSeq } ->
                                    go <|
                                        rest
                                            ++ List.indexedMap (\i x -> ( x, InConditionTrue i :: todoPath )) trueSeq
                                            ++ List.indexedMap (\i x -> ( x, InConditionFalse i :: todoPath )) falseSeq
    in
    go [ ( rootFlow, [] ) ]


find : (Flow node -> Bool) -> Flow node -> Maybe ( Flow node, List Step )
find pred =
    findMap
        (\flow ->
            if pred flow then
                Just flow

            else
                Nothing
        )



-- `Transform` library helpers


{-| Basically a list of all the little optimizations, then combined into
a single function.
-}
optimizeAll : Flow node -> Maybe (Flow node)
optimizeAll =
    Transform.orList
        [ optimizeNestedSequential
        , optimizeNestedParallel
        , optimizeEmptySequentialInParallel
        , optimizeSingletonSequential
        , optimizeSingletonParallel
        , optimizeNormalizeEmpty
        , optimizeSeqInConditionTrue
        , optimizeSeqInConditionFalse
        ]


unwrapSequential : Flow node -> List (Flow node)
unwrapSequential x =
    case x of
        Sequential xs ->
            xs

        _ ->
            [ x ]


optimizeNestedSequential : Flow node -> Maybe (Flow node)
optimizeNestedSequential flow =
    case flow of
        Sequential xs ->
            if List.any isSequential xs then
                Just <| Sequential <| List.concatMap unwrapSequential xs

            else
                Nothing

        _ ->
            Nothing


unwrapParallel : Flow node -> List (Flow node)
unwrapParallel x =
    case x of
        Parallel xs ->
            xs

        _ ->
            [ x ]


optimizeNestedParallel : Flow node -> Maybe (Flow node)
optimizeNestedParallel flow =
    case flow of
        Parallel xs ->
            if List.any isParallel xs then
                Just <| Parallel <| List.concatMap unwrapParallel xs

            else
                Nothing

        _ ->
            Nothing


optimizeEmptySequentialInParallel : Flow node -> Maybe (Flow node)
optimizeEmptySequentialInParallel flow =
    case flow of
        Parallel xs ->
            if List.any (\x -> isSequential x && isEmpty x) xs then
                Just <| Parallel <| List.filter (\x -> not (isEmpty x)) xs

            else
                Nothing

        _ ->
            Nothing


optimizeSingletonSequential : Flow node -> Maybe (Flow node)
optimizeSingletonSequential flow =
    case flow of
        Sequential [ x ] ->
            Just x

        _ ->
            Nothing


optimizeSingletonParallel : Flow node -> Maybe (Flow node)
optimizeSingletonParallel flow =
    case flow of
        Parallel [ x ] ->
            Just x

        _ ->
            Nothing


{-| Sequential [] is our go-to empty Flow representation.
Let's coerce other empty states into that.
-}
optimizeNormalizeEmpty : Flow node -> Maybe (Flow node)
optimizeNormalizeEmpty flow =
    case flow of
        Parallel [] ->
            Just <| Sequential []

        _ ->
            Nothing


optimizeSeqInConditionTrue : Flow node -> Maybe (Flow node)
optimizeSeqInConditionTrue flow =
    case flow of
        Condition n r ->
            if List.any isSequential r.trueSeq then
                Just <| Condition n { r | trueSeq = List.concatMap unwrapSequential r.trueSeq }

            else
                Nothing

        _ ->
            Nothing


optimizeSeqInConditionFalse : Flow node -> Maybe (Flow node)
optimizeSeqInConditionFalse flow =
    case flow of
        Condition n r ->
            if List.any isSequential r.falseSeq then
                Just <| Condition n { r | falseSeq = List.concatMap unwrapSequential r.falseSeq }

            else
                Nothing

        _ ->
            Nothing


{-| A helper for Transform.transformAll: teaches it how to traverse our
data structure.
-}
recurse : (Flow node -> Flow node) -> Flow node -> Flow node
recurse fn flow =
    case flow of
        Node node ->
            Node node

        Sequential xs ->
            Sequential (List.map fn xs)

        Parallel xs ->
            Parallel (List.map fn xs)

        Condition q { trueSeq, falseSeq } ->
            Condition q
                { trueSeq = List.map fn trueSeq
                , falseSeq = List.map fn falseSeq
                }


{-| A helper for Transform.children: teaches it how to traverse our
data structure.
-}
recursiveChildren : (Flow node -> List (Flow node)) -> Flow node -> List (Flow node)
recursiveChildren fn flow =
    case flow of
        Node _ ->
            []

        Sequential xs ->
            List.concatMap fn xs

        Parallel xs ->
            List.concatMap fn xs

        Condition _ { trueSeq, falseSeq } ->
            List.concatMap fn trueSeq ++ List.concatMap fn falseSeq


sortParallel : { toPosition : node -> comparable } -> Flow node -> Flow node
sortParallel { toPosition } flow =
    Transform.transformOnce
        recurse
        (\f ->
            case f of
                Parallel xs ->
                    Parallel (List.sortBy (nodes >> List.map (Tuple.first >> toPosition)) xs)

                _ ->
                    f
        )
        flow

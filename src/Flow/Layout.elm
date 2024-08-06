module Flow.Layout exposing
    ( Layout
    , layout
    , Pos
    , Config, NodeProperties, ConnectionEnding(..)
    , Box
    , ConditionRoot
    , mapNodes
    , Size
    )

{-|

@docs Layout
@docs layout
@docs Pos
@docs Config, NodeProperties, ConnectionEnding
@docs Box
@docs ConditionRoot
@docs mapNodes

-}

import Flow exposing (Flow(..))
import Flow.Line exposing (Path)
import List.Extra
import Maybe.Extra


{-| -}
type alias Config node =
    { nodeProperties : node -> NodeProperties
    , connectionY : ConnectionEnding node -> Int
    , gapX : Int
    , gapY : Int
    , conditionBranchGapX : Int
    , conditionBranchGapY : Int
    , isConditionRoot : node -> Bool
    }


type alias NodeProperties =
    { width : Int
    , height : Int
    , marginLeft : Int
    , marginRight : Int
    }


{-| -}
type alias Layout node =
    { -- Each node and their computed position and size
      nodes : List ( node, Box )

    -- SVG paths that connect all the node boxes
    , connections : List ( ConnectionEnding node, Path, ConnectionEnding node )

    -- Top-left box on each condition branch
    -- Useful for "True"/"False" branch labels or other decorations
    , conditionRoots : List (ConditionRoot node)

    -- Total size
    , size : Size

    -- Leftmost box on each parallel track
    , left : List ( ConnectionEnding node, Box )

    -- Rightmost box on each parallel track
    , right : List ( ConnectionEnding node, Box )
    }


{-| Connections are not only drawn between nodes but also branch endings.

If a branch starts or ends with a parallel, we do not want
each of the tracks to connect to the condition node directly:

                  True
    Condition ──╮────── A
                │
                │ False
                ├────── B
                │
                ╰────── C

Instead we want the condition to connect to an "ending" that represents
the start of the branch and from which each parallel track starts:

                  True
    Condition ──╮────── A
                │
                │ False
                ╰──┬─── B
                   │
                   ╰─── C

This step is only necessary if either of the branch endings are parallel.

-}
type ConnectionEnding node
    = BranchStart
    | BranchEnd
    | CNode node


{-| -}
type alias Pos =
    { x : Int
    , y : Int
    }


{-| -}
type alias Box =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , connectionY : Int

    -- The x position before margins were applied
    -- This is useful for drawing connections
    , marginBoxX : Int
    }


{-| -}
type alias Size =
    { width : Int
    , height : Int
    }


{-| -}
type alias ConditionRoot node =
    { condition : node
    , branch : Bool
    , box : Box
    }


{-| Compute a Layout from a Flow tree.
-}
layout : Pos -> Config node -> Flow node -> Layout node
layout pos config flow =
    case flow of
        Node node ->
            let
                { width, height, marginLeft, marginRight } =
                    config.nodeProperties node

                box : Box
                box =
                    { x = pos.x + marginLeft
                    , y = pos.y
                    , width = width
                    , height = height
                    , connectionY = config.connectionY (CNode node)
                    , marginBoxX = pos.x
                    }

                side =
                    [ ( CNode node, box ) ]

                size =
                    { width = box.x + box.width + marginRight
                    , height = box.y + box.height
                    }
            in
            { nodes = [ ( node, box ) ]
            , connections = []
            , conditionRoots = []
            , size = size
            , left = side
            , right = side
            }

        Sequential items ->
            collection advanceSeq pos config items

        Parallel items ->
            collection advancePar pos config items

        Condition node branches ->
            condition pos config node branches


{-| Apply a function to every node in the layout.
-}
mapNodes : (a -> b) -> Layout a -> Layout b
mapNodes fn lay =
    { nodes = List.map (Tuple.mapFirst fn) lay.nodes
    , connections =
        List.map
            (\( a, path, b ) ->
                ( mapConnectionEnding fn a
                , path
                , mapConnectionEnding fn b
                )
            )
            lay.connections
    , conditionRoots =
        List.map
            (\root ->
                { condition = fn root.condition
                , branch = root.branch
                , box = root.box
                }
            )
            lay.conditionRoots
    , size = lay.size
    , left = List.map (Tuple.mapFirst (mapConnectionEnding fn)) lay.left
    , right = List.map (Tuple.mapFirst (mapConnectionEnding fn)) lay.right
    }


mapConnectionEnding : (a -> b) -> ConnectionEnding a -> ConnectionEnding b
mapConnectionEnding fn ending =
    case ending of
        BranchStart ->
            BranchStart

        BranchEnd ->
            BranchEnd

        CNode node ->
            CNode (fn node)



-- COLLECTIONS


collection :
    (Pos -> Config node -> Flow node -> Layout node -> Layout node)
    -> Pos
    -> Config node
    -> List (Flow node)
    -> Layout node
collection fn pos config =
    List.foldl (fn pos config)
        { nodes = []
        , connections = []
        , conditionRoots = []
        , size = { width = pos.x, height = pos.y }
        , left = []
        , right = []
        }


advanceSeq : Pos -> Config node -> Flow node -> Layout node -> Layout node
advanceSeq pos config flow lay =
    let
        gap : Int
        gap =
            if List.isEmpty lay.nodes then
                0

            else
                config.gapX

        new : Layout node
        new =
            layout { pos | x = lay.size.width + gap } config flow

        connections : List ( ConnectionEnding node, Path, ConnectionEnding node )
        connections =
            let
                anchor : Maybe ( ConnectionEnding node, Box )
                anchor =
                    if List.length lay.right > 1 && List.length new.left > 1 then
                        -- Many to many connection.
                        -- Use highest node as anchor
                        List.Extra.minimumBy (\( _, b ) -> b.y) new.left

                    else
                        Nothing
            in
            -- Connect leftmost boxes of this step to rightmost boxes of last step
            List.concatMap
                (\trackLeft ->
                    List.map (connect config trackLeft anchor) lay.right
                )
                new.left
    in
    { nodes = lay.nodes ++ new.nodes
    , size = maxSize lay.size new.size
    , connections = connections ++ new.connections ++ lay.connections
    , conditionRoots = new.conditionRoots ++ lay.conditionRoots

    -- We are laying out nodes horizontally
    , left =
        -- Only the first step is the leftmost
        if List.isEmpty lay.left then
            new.left

        else
            lay.left

    -- Last step is rightmost
    , right = new.right
    }


advancePar : Pos -> Config node -> Flow node -> Layout node -> Layout node
advancePar pos config flow lay =
    let
        gap : Int
        gap =
            if List.isEmpty lay.nodes then
                0

            else if hasConditionInTopRow flow then
                config.conditionBranchGapY

            else
                config.gapY

        new : Layout node
        new =
            layout { pos | y = lay.size.height + gap } config flow
    in
    { nodes = lay.nodes ++ new.nodes
    , size = maxSize lay.size new.size
    , connections = new.connections ++ lay.connections
    , conditionRoots = new.conditionRoots ++ lay.conditionRoots

    -- Each step is a parallel track, append their leftmost and rightmost boxes
    -- Order is not significant
    , left = new.left ++ lay.left
    , right = new.right ++ lay.right
    }


{-| Determines whether any of the nodes in the top parallel row are a condition
-}
hasConditionInTopRow : Flow node -> Bool
hasConditionInTopRow flow =
    case flow of
        Node _ ->
            False

        Condition _ _ ->
            True

        Sequential items ->
            List.any hasConditionInTopRow items

        Parallel (top :: _) ->
            hasConditionInTopRow top

        Parallel [] ->
            False


maxSize : Size -> Size -> Size
maxSize a b =
    { width = max a.width b.width
    , height = max a.height b.height
    }



-- CONNECTIONS


connect :
    Config node
    -> ( ConnectionEnding node, Box )
    -> Maybe ( ConnectionEnding node, Box )
    -> ( ConnectionEnding node, Box )
    -> ( ConnectionEnding node, Path, ConnectionEnding node )
connect config ( b, bBox ) maybeAnchor ( a, aBox ) =
    let
        aMidY : Int
        aMidY =
            aBox.y + aBox.connectionY

        bMidY : Int
        bMidY =
            bBox.y + bBox.connectionY

        middle : List Pos
        middle =
            case maybeAnchor of
                Nothing ->
                    [ { x = bBox.marginBoxX - (config.gapX // 2)
                      , y = bMidY
                      }
                    ]

                Just ( anchor, anchorBox ) ->
                    [ { x = anchorBox.marginBoxX - (config.gapX // 4 * 3)
                      , y = anchorBox.y + config.connectionY anchor
                      }
                    , { x = bBox.marginBoxX - (config.gapX // 4)
                      , y = bMidY
                      }
                    ]

        path : Path
        path =
            Flow.Line.Horizontal
                -- Origin
                { x = aBox.x
                , y = aMidY
                }
                middle
                -- Destination
                { x = bBox.x
                , y = bMidY
                }
    in
    ( a, path, b )



-- CONDITIONS


condition :
    Pos
    -> Config node
    -> node
    -> { trueSeq : List (Flow node), falseSeq : List (Flow node) }
    -> Layout node
condition pos config node { trueSeq, falseSeq } =
    let
        { width, height, marginLeft, marginRight } =
            config.nodeProperties node

        condBox : Box
        condBox =
            { x = pos.x + marginLeft
            , y = pos.y
            , width = width
            , height = height
            , marginBoxX = pos.x
            , connectionY = config.connectionY (CNode node)
            }

        cond =
            ( CNode node, condBox )

        afterCondSize : Size
        afterCondSize =
            { width = condBox.x + condBox.width + marginRight
            , height = condBox.y + condBox.height
            }

        afterCondX : Int
        afterCondX =
            afterCondSize.width + max config.gapX config.conditionBranchGapX

        addBranchEndings =
            hasParallelEndings trueSeq || hasParallelEndings falseSeq

        true =
            conditionBranch
                { pos =
                    { x = afterCondX
                    , y = pos.y
                    }
                , config = config
                , seq = trueSeq
                , addEndings = addBranchEndings
                }

        trueFalseGap : Int
        trueFalseGap =
            if List.isEmpty true.layout.nodes then
                0

            else
                config.conditionBranchGapY

        false =
            conditionBranch
                { pos =
                    { x = afterCondX
                    , y = true.layout.size.height + trueFalseGap
                    }
                , config = config
                , seq = falseSeq
                , addEndings = addBranchEndings
                }

        roots : List (ConditionRoot node)
        roots =
            conditionRoot config node True true
                ++ conditionRoot config node False false

        connections : List ( ConnectionEnding node, Path, ConnectionEnding node )
        connections =
            true.layout.left
                ++ false.layout.left
                |> List.map (\trackLeft -> connect config trackLeft Nothing cond)
    in
    { nodes = ( node, condBox ) :: true.layout.nodes ++ false.layout.nodes
    , connections = connections ++ true.layout.connections ++ false.layout.connections
    , conditionRoots = roots ++ true.layout.conditionRoots ++ false.layout.conditionRoots
    , size =
        afterCondSize
            |> maxSize true.layout.size
            |> maxSize false.layout.size
    , left = [ cond ]
    , right = true.layout.right ++ false.layout.right
    }


hasParallelEndings : List (Flow node) -> Bool
hasParallelEndings seq =
    case List.head seq of
        Just first ->
            if Flow.isParallel first then
                True

            else
                case List.Extra.last seq of
                    Just last ->
                        Flow.isParallel last

                    Nothing ->
                        False

        Nothing ->
            False


type alias ConditionBranchLayout node =
    { layout : Layout node
    , topLeftBox : Maybe Box
    }


conditionBranch :
    { pos : Pos
    , config : Config node
    , seq : List (Flow node)
    , addEndings : Bool
    }
    -> ConditionBranchLayout node
conditionBranch { pos, config, seq, addEndings } =
    if not addEndings then
        let
            lay =
                collection advanceSeq pos config seq
        in
        { layout = lay
        , topLeftBox = topBox lay.left
        }

    else
        let
            lay =
                collection
                    advanceSeq
                    { x = pos.x + config.gapX
                    , y = pos.y
                    }
                    config
                    seq

            left =
                case topBox lay.left of
                    Just box ->
                        let
                            x =
                                box.marginBoxX - config.gapX

                            phantomNode =
                                ( BranchStart
                                , { x = x
                                  , y = box.y
                                  , width = 0
                                  , height = 0
                                  , connectionY = config.connectionY BranchStart
                                  , marginBoxX = x
                                  }
                                )
                        in
                        { topBox = Just box
                        , side = [ phantomNode ]
                        , connections = List.map (\track -> connect config track Nothing phantomNode) lay.left
                        }

                    Nothing ->
                        { topBox = Nothing
                        , side = []
                        , connections = []
                        }

            right =
                case topBox lay.right of
                    Just box ->
                        let
                            x =
                                lay.size.width + config.gapX

                            phantomNode =
                                ( BranchEnd
                                , { x = x
                                  , y = box.y
                                  , width = 0
                                  , height = 0
                                  , connectionY = config.connectionY BranchEnd
                                  , marginBoxX = x
                                  }
                                )
                        in
                        { side = [ phantomNode ]
                        , connections = List.map (connect config phantomNode Nothing) lay.right
                        }

                    Nothing ->
                        { side = []
                        , connections = []
                        }
        in
        { layout =
            { lay
                | left = left.side
                , right = right.side
                , connections = left.connections ++ right.connections ++ lay.connections
                , size =
                    { width = lay.size.width + config.gapX
                    , height = lay.size.height
                    }
            }
        , topLeftBox = left.topBox
        }


topBox : List ( node, Box ) -> Maybe Box
topBox =
    List.Extra.minimumBy (Tuple.second >> .y) >> Maybe.map Tuple.second


conditionRoot : Config node -> node -> Bool -> ConditionBranchLayout node -> List (ConditionRoot node)
conditionRoot config cond branch condLay =
    let
        maybeBox : Maybe Box
        maybeBox =
            condLay.layout.nodes
                |> List.Extra.find (\( node, _ ) -> config.isConditionRoot node)
                |> Maybe.map (\( _, box ) -> box)
                |> Maybe.Extra.orElse condLay.topLeftBox
    in
    case maybeBox of
        Just box ->
            [ { condition = cond
              , branch = branch
              , box = box
              }
            ]

        Nothing ->
            []

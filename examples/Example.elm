module Example exposing (..)

{-| @story
@covers Flow.Layout
-}

import Flow exposing (Flow(..))
import Flow.Layout
import Flow.Line
import Html
import Html.Attributes as Attr
import Svg
import Svg.Attributes as SvgAttr



-- import Ui
-- import Ui.Text


main : Html.Html msg
main =
    view
        { gapX = 40
        , gapY = 40
        , conditionBranchGapX = 40
        , conditionBranchGapY = 40
        , nodeWidth = 100
        , nodeHeight = 40
        , nodeMarginLeft = 20
        , nodeMarginRight = 20
        , nodeConnectionY = 20
        , connectionRadius = 8
        }



-- CONTROLS


type alias Args =
    { gapX : Int
    , gapY : Int
    , conditionBranchGapX : Int
    , conditionBranchGapY : Int
    , nodeWidth : Int
    , nodeHeight : Int
    , nodeMarginLeft : Int
    , nodeMarginRight : Int
    , nodeConnectionY : Int
    , connectionRadius : Int
    }



-- VIEW


view : Args -> Html.Html msg
view args =
    let
        layout =
            flow
                |> Flow.Layout.layout { x = 40, y = 40 }
                    { nodeProperties =
                        always
                            { width = args.nodeWidth
                            , height = args.nodeHeight
                            , marginLeft = args.nodeMarginLeft
                            , marginRight = args.nodeMarginRight
                            }
                    , connectionY = always (min args.nodeHeight args.nodeConnectionY)
                    , gapX = args.gapX
                    , gapY = args.gapY
                    , conditionBranchGapX = args.conditionBranchGapX
                    , conditionBranchGapY = args.conditionBranchGapY
                    , isConditionRoot = \_ -> True
                    }

        nodes =
            layout.nodes
                |> List.map
                    (\( node, box ) ->
                        Html.div
                            [ Attr.style "width" (String.fromInt box.width ++ "px")
                            , Attr.style "height" (String.fromInt box.height ++ "px")
                            , translate box.x box.y
                            , Attr.style "background-color" "pink"
                            , Attr.style "border-radius" "8px"
                            , Attr.style "position" "absolute"
                            , Attr.style "left" "0"
                            , Attr.style "top" "0"
                            , Attr.style "display" "flex"
                            , Attr.style "align-items" "center"
                            , Attr.style "justify-content" "center"
                            ]
                            [ Html.span [] [ Html.text node ] ]
                    )

        connections =
            layout.connections
                |> List.map
                    (\( _, path, _ ) ->
                        Flow.Line.view []
                            { stroke = Flow.Line.Solid
                            , strokeWidth = 1
                            , strokeColor = Just "black"
                            , cornerRadius = args.connectionRadius
                            }
                            path
                    )
                |> Svg.svg
                    [ SvgAttr.style "position: absolute; pointer-events: none; left: 0; top: 0; width: 100%; height: 100%;"
                    ]

        labels =
            layout.conditionRoots
                |> List.map
                    (\{ branch, box } ->
                        Html.div
                            [ translate box.x (box.y - 20)

                            -- , Ui.fontColor.greyscale700
                            , Attr.style "position" "absolute"
                            , Attr.style "left" "0"
                            , Attr.style "top" "0"
                            , Attr.style "display" "flex"
                            , Attr.style "align-items" "center"
                            , Attr.style "justify-content" "center"
                            ]
                            [ Html.text
                                (if branch then
                                    "True"

                                 else
                                    "False"
                                )
                            ]
                    )
    in
    Html.div [ Attr.style "width" "100vw", Attr.style "height" "100vh" ]
        [ connections
            :: labels
            ++ nodes
            |> Html.div
                [ Attr.style "width" (String.fromInt layout.size.width ++ "px")
                , Attr.style "height" (String.fromInt layout.size.height ++ "px")
                , Attr.style "position" "relative"
                ]
        ]


translate : Int -> Int -> Html.Attribute msg
translate x y =
    Attr.style "translate" (String.fromInt x ++ "px " ++ String.fromInt y ++ "px")


flow : Flow String
flow =
    Sequential
        [ Node "1"
        , Node "2"
        , Parallel
            [ Sequential
                [ Parallel
                    [ Node "3"
                    , Node "4"
                    ]
                , Parallel
                    [ Node "A 3"
                    , Node "A 4"
                    ]
                , Node "5"
                ]
            , Node "6"
            ]
        , cond
        , Node "7"
        , Node "8"
        ]


cond : Flow String
cond =
    Condition "?"
        { trueSeq = [ Parallel [ Node "T", Node "A" ], Node "B" ]
        , falseSeq =
            [ Parallel
                [ Sequential [ Node "I", Node "II" ]
                , Sequential [ Node "III", Node "IV" ]
                ]
            ]
        }

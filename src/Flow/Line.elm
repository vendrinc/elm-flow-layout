module Flow.Line exposing
    ( Path(..)
    , Point
    , Stroke(..)
    , toCommands
    , view
    )

{-| -}

import Html exposing (Html)
import Svg
import Svg.Attributes as SvgAttr


type Path
    = Horizontal Point (List Point) Point
    | Vertical Point Point


type alias Point =
    { x : Int
    , y : Int
    }


type Stroke
    = Dashed Int
    | Solid


view :
    List (Svg.Attribute msg)
    ->
        { stroke : Stroke
        , strokeWidth : Int
        , strokeColor : Maybe String
        , cornerRadius : Int
        }
    -> Path
    -> Html msg
view extraAttrs cfg path =
    let
        withPathStyle attrs_ =
            case cfg.stroke of
                Dashed i ->
                    SvgAttr.strokeDasharray (String.fromInt i)
                        :: attrs_

                Solid ->
                    attrs_

        withStrokeClassName attrs_ =
            case cfg.strokeColor of
                Just strokeColor ->
                    SvgAttr.stroke strokeColor :: attrs_

                Nothing ->
                    attrs_

        commands : String
        commands =
            toCommands cfg.cornerRadius path

        attrs : List (Svg.Attribute msg)
        attrs =
            [ SvgAttr.d commands
            , SvgAttr.strokeWidth (String.fromInt cfg.strokeWidth)
            , SvgAttr.fill "transparent"
            , SvgAttr.strokeLinecap "round"
            ]
                |> withPathStyle
                |> withStrokeClassName
    in
    Svg.path (attrs ++ extraAttrs) []


toCommands : Int -> Path -> String
toCommands radius path =
    case path of
        Horizontal start middle end ->
            let
                next : Point -> ( Point, String ) -> ( Point, String )
                next current ( previous, d ) =
                    ( current, d ++ " " ++ horizontal radius previous current )

                initial : ( Point, String )
                initial =
                    ( start, moveTo start )

                ( _, pathD ) =
                    middle
                        |> List.foldl next initial
                        |> next end
            in
            pathD

        Vertical start end ->
            moveTo start ++ " " ++ vertical radius start end


horizontal : Int -> Point -> Point -> String
horizontal radius one two =
    if one.y == two.y then
        {-
           Start ──── End
        -}
        lineTo two

    else
        {-


                   4 ╭─ End
                     │
                     │ 3
                     │
           Start ────╯ 2
                   1
        -}
        let
            cornerRadius : Int
            cornerRadius =
                radius
                    |> min (abs (one.y - two.y))
                    |> min (abs (one.x - two.x))

            turnOne : Point
            turnOne =
                { x = two.x - cornerRadius
                , y = one.y
                }

            turnTwo : Point
            turnTwo =
                { x = two.x
                , y =
                    if one.y > two.y then
                        two.y + cornerRadius

                    else
                        two.y - cornerRadius
                }
        in
        String.join " "
            [ -- 1
              lineTo turnOne

            -- 2
            , curveTo cornerRadius
                { x = two.x
                , y = one.y
                }
                turnTwo

            -- 3
            , lineTo turnTwo

            -- 4
            , horizontalCurveTo cornerRadius
                two
                { x = two.x + cornerRadius
                , y = two.y
                }
            ]


vertical : Int -> Point -> Point -> String
vertical radius one two =
    let
        cornerRadius : Int
        cornerRadius =
            radius
                |> min (abs (one.y - two.y) // 2)
                |> min (abs (one.x - two.x) // 2)

        extra : Int
        extra =
            cornerRadius // 2
    in
    if one.x == two.x then
        {-
           Start
            │
            │
           End
        -}
        lineTo two

    else if one.x + extra > two.x then
        {-
                        1
                 Start ──╮ 2
                   5     │ 3
           6 ╭───────────╯ 4
           7 │
            End
        -}
        let
            centerY : Int
            centerY =
                getMiddle one.y two.y

            turnOne : Point
            turnOne =
                { x = one.x + extra + cornerRadius
                , y =
                    if one.y > two.y then
                        centerY + cornerRadius

                    else
                        centerY - cornerRadius
                }

            turnTwo : Point
            turnTwo =
                { x = two.x + cornerRadius
                , y = centerY
                }
        in
        String.join " "
            [ -- 1
              lineTo
                { x = one.x + extra
                , y = one.y
                }

            -- 2
            , curveTo cornerRadius
                { x = turnOne.x
                , y = one.y
                }
                turnOne

            -- 3
            , lineTo turnOne

            -- 4
            , horizontalCurveTo cornerRadius
                { x = turnOne.x
                , y = centerY
                }
                turnTwo

            -- 5
            , lineTo turnTwo

            -- 6
            , curveTo cornerRadius
                { x = two.x
                , y = centerY
                }
                two

            -- 7
            , lineTo two
            ]

    else
        {-
           Start ───────────╮
                            │
                           End
        -}
        String.join " "
            [ lineTo
                { x = two.x - cornerRadius
                , y = one.y
                }
            , curveTo cornerRadius
                { x = two.x
                , y = one.y
                }
                two
            , lineTo two
            ]


getMiddle : Int -> Int -> Int
getMiddle one two =
    if one > two then
        ((one - two) // 2) + two

    else
        ((two - one) // 2) + one


lineTo : Point -> String
lineTo point =
    "L " ++ renderPoint point


moveTo : Point -> String
moveTo point =
    "M " ++ renderPoint point


curveTo : Int -> Point -> Point -> String
curveTo cornerRadius control target =
    "S "
        ++ renderPoint control
        ++ " "
        ++ renderPoint
            { x = control.x
            , y =
                if target.y > control.y then
                    control.y + cornerRadius

                else
                    control.y - cornerRadius
            }


horizontalCurveTo : Int -> Point -> Point -> String
horizontalCurveTo cornerRadius control target =
    "S "
        ++ renderPoint control
        ++ " "
        ++ renderPoint
            { x =
                if target.x > control.x then
                    control.x + cornerRadius

                else
                    control.x - cornerRadius
            , y =
                control.y
            }


renderPoint : { x : Int, y : Int } -> String
renderPoint point =
    String.fromInt point.x ++ "," ++ String.fromInt point.y

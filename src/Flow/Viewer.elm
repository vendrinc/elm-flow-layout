module Flow.Viewer exposing
    ( Model, Msg
    , init, update, subscriptions
    , focusOn, focusOnWith
    , seeAll, seeAllWith
    , setOffset, Offset
    , zoomCenter
    , withTransitions
    , setViewportBox
    , setLayout
    , view, BranchLabel, Lazy, Lazy4
    , viewSolidConnection
    , cssNodeId, cssBranchLabelId
    , toViewportCoords
    , resizeMsgBox, MeasureState
    )

{-|

@docs Model, Msg
@docs init, update, subscriptions
@docs focusOn, focusOnWith
@docs seeAll, seeAllWith
@docs setOffset, Offset
@docs zoomCenter
@docs withTransitions
@docs setViewportBox
@docs setLayout
@docs view, BranchLabel, Lazy, Lazy4
@docs viewSolidConnection
@docs cssNodeId, cssBranchLabelId
@docs toViewportCoords

@docs resizeMsgBox, MeasureState

-}

import Browser.Events
import Flow.Layout exposing (ConnectionEnding, Layout)
import Flow.Line
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Html.Keyed
import Html.Lazy
import Json.Decode
import List.Extra
import Svg
import Svg.Attributes as SvgAttr


{-| -}
type alias Model node =
    { zoom : Float
    , offset : Offset
    , viewportBox : MeasureState
    , panning : Bool
    , layout : Layout ( String, node )
    , transitions : Bool
    }


{-| -}
type alias Box =
    { left : Float
    , top : Float
    , right : Float
    , bottom : Float
    , width : Float
    , height : Float
    }


{-| -}
type MeasureState
    = Measured Box
    | AwaitingMeasure
    | AwaitingMeasureAndFocus String


{-| -}
type alias Offset =
    { x : Float
    , y : Float
    }


{-| -}
init :
    { toId : node -> String
    , viewportBox : Maybe Box
    }
    -> Layout node
    -> Model node
init options layout =
    { zoom = 1
    , offset = { x = 0, y = 0 }
    , viewportBox =
        case options.viewportBox of
            Just box ->
                Measured box

            Nothing ->
                AwaitingMeasure
    , panning = False
    , layout =
        layout
            |> Flow.Layout.mapNodes (\node -> ( options.toId node, node ))
    , transitions = False
    }


{-| -}
type Msg
    = GotViewportBox Box
    | Pinched { gapDelta : Float, mouseX : Float, mouseY : Float }
    | Scrolled { deltaX : Float, deltaY : Float }
    | Panned { movementX : Float, movementY : Float }
    | PanStarted
    | PanEnded


{-| -}
resizeMsgBox : Msg -> Maybe Box
resizeMsgBox msg =
    case msg of
        GotViewportBox box ->
            Just box

        _ ->
            Nothing


{-| -}
update : Msg -> Model node -> Model node
update msg model =
    case msg of
        GotViewportBox newBox ->
            let
                newModel : Model node
                newModel =
                    { model | viewportBox = Measured newBox }
            in
            case model.viewportBox of
                AwaitingMeasure ->
                    seeAll newModel

                AwaitingMeasureAndFocus id ->
                    focusOn { id = id } newModel

                Measured oldBox ->
                    let
                        correctionX : Float
                        correctionX =
                            (oldBox.left - newBox.left) / model.zoom

                        correctionY : Float
                        correctionY =
                            (oldBox.top - newBox.top) / model.zoom
                    in
                    newModel
                        |> setOffset
                            { x = model.offset.x + correctionX
                            , y = model.offset.y + correctionY
                            }

        Pinched { gapDelta, mouseX, mouseY } ->
            zoomWithFocus (model.zoom - gapDelta * 0.005)
                (\viewport ->
                    { x = mouseX - viewport.left
                    , y = mouseY - viewport.top
                    }
                )
                model

        Scrolled { deltaX, deltaY } ->
            setOffset
                { x = model.offset.x - (deltaX / model.zoom)
                , y = model.offset.y - (deltaY / model.zoom)
                }
                model

        Panned { movementX, movementY } ->
            if model.panning then
                setOffset
                    { x = model.offset.x + (movementX / model.zoom)
                    , y = model.offset.y + (movementY / model.zoom)
                    }
                    model

            else
                model

        PanStarted ->
            { model | panning = True }

        PanEnded ->
            { model | panning = False }


{-| -}
subscriptions : Model node -> (Msg -> msg) -> Maybe (Int -> Int -> Json.Decode.Decoder msg) -> Sub msg
subscriptions model toMsg onMouseMove =
    if model.panning then
        Sub.batch
            [ Browser.Events.onMouseMove (Json.Decode.map toMsg panningEventDecoder)
            , Browser.Events.onMouseUp (Json.Decode.succeed (toMsg PanEnded))
            ]

    else
        case onMouseMove of
            Just decodeMouseMove ->
                let
                    viewport =
                        viewportBox model
                in
                mouseMoveEventDecoder
                    |> Json.Decode.andThen
                        (\( x, y ) ->
                            decodeMouseMove
                                (round ((x - viewport.left) / model.zoom - model.offset.x))
                                (round ((y - viewport.top) / model.zoom - model.offset.y))
                        )
                    |> Browser.Events.onMouseMove

            Nothing ->
                Sub.none


{-| Update layout but keep the same zoom and offset.
-}
setLayout :
    { toId : node -> String
    , layout : Layout node
    }
    -> Model node
    -> Model node
setLayout { toId, layout } model =
    { model | layout = Flow.Layout.mapNodes (\node -> ( toId node, node )) layout }


{-| Set viewport box explicitly.

Note: Resizes will override this.

-}
setViewportBox : Box -> Model node -> Model node
setViewportBox box model =
    { model | viewportBox = Measured box }


{-| Set the zoom level while focusing on the center of the viewport.
-}
zoomCenter : (Float -> Float) -> Model node -> Model node
zoomCenter toLevel model =
    zoomWithFocus (toLevel model.zoom)
        (\viewport ->
            { x = viewport.width / 2
            , y = viewport.height / 2
            }
        )
        model


zoomWithFocus : Float -> (Box -> Offset) -> Model node -> Model node
zoomWithFocus zoom toOffset model =
    let
        oldZoom : Float
        oldZoom =
            model.zoom

        newZoom : Float
        newZoom =
            zoom
                |> clamp 0.5 2

        correction : Float -> Float
        correction mouse =
            -- Calculate the shift in positions caused by the scale change
            (mouse / newZoom)
                - (mouse / oldZoom)

        viewport : Box
        viewport =
            viewportBox model

        offset : Offset
        offset =
            toOffset viewport

        newOffset : Offset
        newOffset =
            { x = model.offset.x + correction offset.x
            , y = model.offset.y + correction offset.y
            }
    in
    { model | zoom = newZoom }
        |> setOffset newOffset


padding : Float
padding =
    50


{-| Set the viewport offset relative to layout dimensions.

Values are limited to the middle point of the viewport to prevent users from scrolling away completely.

-}
setOffset : Offset -> Model node -> Model node
setOffset offset model =
    let
        container : Box
        container =
            viewportBox model

        midContainerX : Float
        midContainerX =
            container.width / model.zoom / 2

        minX : Float
        minX =
            toFloat -model.layout.size.width
                + midContainerX

        midContainerY : Float
        midContainerY =
            container.height / model.zoom / 2

        minY : Float
        minY =
            toFloat -model.layout.size.height
                + midContainerY
    in
    { model
        | offset =
            { x = offset.x |> clamp minX midContainerX
            , y = offset.y |> clamp minY midContainerY
            }
        , transitions = False
    }


viewportBox : Model node -> Box
viewportBox model =
    case model.viewportBox of
        Measured box ->
            box

        _ ->
            { left = 0
            , top = 0
            , right = 0
            , bottom = 0
            , width = 0
            , height = 0
            }


{-| Center and zoom as much as necessary to fit the whole content if possible.
-}
seeAll : Model node -> Model node
seeAll =
    seeAllWith { bounded = True, offsetY = 0 }


{-| -}
seeAllWith : { bounded : Bool, offsetY : Float } -> Model node -> Model node
seeAllWith { bounded, offsetY } model =
    let
        viewport : Box
        viewport =
            viewportBox model

        width : Float
        width =
            toFloat model.layout.size.width

        height : Float
        height =
            toFloat model.layout.size.height

        zoom : Float
        zoom =
            min
                (viewport.width / (width + padding * 2))
                (viewport.height / (height + padding * 2))
                |> (if bounded then
                        clamp 0.5 1.0

                    else
                        identity
                   )

        offset : Offset
        offset =
            { x = viewport.width / zoom / 2 - width / 2
            , y = viewport.height / zoom / 2 - height / 2 + offsetY / zoom
            }
    in
    { model
        | offset = offset
        , zoom = zoom
        , transitions = False
    }


{-| Center on a specific node by id. Zoom remains the same.
-}
focusOn : { id : String } -> Model node -> Model node
focusOn { id } =
    focusOnWith
        { id = id
        , onlyHorizontally = False
        , offsetX = 0
        , offsetY = 0
        }


{-| -}
focusOnWith : { id : String, onlyHorizontally : Bool, offsetX : Int, offsetY : Int } -> Model node -> Model node
focusOnWith { id, onlyHorizontally, offsetX, offsetY } model =
    case model.viewportBox of
        Measured viewport ->
            let
                found =
                    List.Extra.find (\( ( nid, _ ), _ ) -> nid == id)
                        model.layout.nodes
            in
            case found of
                Just ( _, box ) ->
                    let
                        midX : Float
                        midX =
                            toFloat box.x + toFloat box.width / 2

                        midY : Float
                        midY =
                            toFloat box.y + toFloat box.height / 2

                        offset : Offset
                        offset =
                            { x = viewport.width / model.zoom / 2 - midX - toFloat offsetX
                            , y =
                                if onlyHorizontally then
                                    model.offset.y - toFloat offsetY

                                else
                                    viewport.height / model.zoom / 2 - midY - toFloat offsetY
                            }
                    in
                    { model
                        | offset = offset
                        , transitions = False
                    }

                Nothing ->
                    model

        AwaitingMeasure ->
            { model | viewportBox = AwaitingMeasureAndFocus id }

        AwaitingMeasureAndFocus _ ->
            { model | viewportBox = AwaitingMeasureAndFocus id }


{-| Transition to new offset or zoom level.
-}
withTransitions : Model node -> Model node
withTransitions model =
    { model | transitions = True }


{-| Convert page coords to viewport coords according to zoom and offset
-}
toViewportCoords : Model node -> Float -> Float -> { x : Int, y : Int }
toViewportCoords model x y =
    let
        viewport =
            viewportBox model
    in
    { x = round ((x - viewport.left) / model.zoom - model.offset.x)
    , y = round ((y - viewport.top) / model.zoom - model.offset.y)
    }


{-| -}
type alias BranchLabel msg =
    { view : Html.Html msg
    , offsetY : Int
    }


{-| -}
type alias Lazy a r =
    { fn : a -> r
    , a : a
    }


{-| -}
type alias Lazy4 a b c d r =
    { fn : a -> b -> c -> d -> r
    , a : a
    , b : b
    , c : c
    , d : d
    }


{-| -}
view :
    { id : String
    , model : Model node
    , toMsg : Msg -> msg

    -- Unfortunately, we need to explicitly provide this because currying `viewNode` would invalidate lazy.
    , viewNode : Lazy4 na nb nc nd (( String, node ) -> Flow.Layout.Box -> Html.Html msg)
    , viewBranchLabel : node -> Bool -> Maybe (BranchLabel msg)
    , viewConnection :
        Lazy
            ca
            (ConnectionEnding ( String, node )
             -> ConnectionEnding ( String, node )
             -> Flow.Line.Path
             -> Html.Html msg
            )
    , scaledSvgExtra : Maybe (List (Html.Html msg))
    }
    -> Html.Html msg
view { id, model, toMsg, viewNode, viewBranchLabel, viewConnection, scaledSvgExtra } =
    let
        msg =
            toMsg

        idPrefix =
            id ++ "-"
    in
    Html.div
        [ Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.id id
        , Event.preventDefaultOn "wheel" (Json.Decode.map (Tuple.mapFirst msg) wheelEventDecoder)
        , Event.onMouseDown (msg PanStarted)
        , Event.onMouseUp (msg PanEnded)
        , Attr.style "overflow" "hidden"
        , Attr.style "position" "relative"
        ]
        [ Html.div
            [ Attr.style "transform-origin" "0 0"
            , Attr.style "transform"
                ("scale("
                    ++ String.fromFloat model.zoom
                    ++ ") translate("
                    ++ String.fromFloat model.offset.x
                    ++ "px, "
                    ++ String.fromFloat model.offset.y
                    ++ "px)"
                )
            , Attr.style "transition" <|
                if model.transitions then
                    "600ms transform ease"

                else
                    "none"
            , Attr.style "position" "relative"
            , Attr.attribute "role" "tree"
            ]
            [ Html.Lazy.lazy3 viewConnections viewConnection.a viewConnection.fn model.layout
            , Html.Lazy.lazy3 viewLabels idPrefix viewBranchLabel model.layout.conditionRoots
            , Html.Lazy.lazy7 viewNodes
                idPrefix
                viewNode.a
                viewNode.b
                viewNode.c
                viewNode.d
                viewNode.fn
                model.layout.nodes
            , case scaledSvgExtra of
                Just scaledExtra ->
                    viewScaledSvg model.layout scaledExtra

                Nothing ->
                    Html.text ""
            ]
        ]


px : Int -> String
px i =
    String.fromInt i ++ "px"


viewNodes :
    String
    -> na
    -> nb
    -> nc
    -> nd
    -> (na -> nb -> nc -> nd -> ( String, node ) -> Flow.Layout.Box -> Html.Html msg)
    -> List ( ( String, node ), Flow.Layout.Box )
    -> Html.Html msg
viewNodes idPrefix na nb nc nd viewNode nodes =
    nodes
        |> List.map
            (\( ( id, _ ) as idAndNode, box ) ->
                ( id
                , Html.div
                    [ Attr.style "width" (px box.width)
                    , Attr.style "height" (px box.height)
                    , Attr.style "user-select" "none"
                    , Attr.style "position" "absolute"
                    , Attr.style "left" (String.fromInt box.x ++ "px")
                    , Attr.style "top" (String.fromInt box.y ++ "px")
                    , Attr.id (idPrefix ++ id)
                    , Attr.attribute "role" "treeitem"
                    , Attr.tabindex 1
                    ]
                    [ viewNode na nb nc nd idAndNode box ]
                )
            )
        |> Html.Keyed.node "div" []


viewConnections :
    ca
    ->
        (ca
         -> ConnectionEnding ( String, node )
         -> ConnectionEnding ( String, node )
         -> Flow.Line.Path
         -> Html.Html msg
        )
    -> Layout ( String, node )
    -> Html.Html msg
viewConnections ca viewConnection layout =
    layout.connections
        |> List.map (\( a, path, b ) -> viewConnection ca a b path)
        |> viewScaledSvg layout


{-| -}
viewSolidConnection : Flow.Line.Path -> Html.Html msg
viewSolidConnection =
    Html.Lazy.lazy
        (Flow.Line.view []
            { stroke = Flow.Line.Solid
            , strokeWidth = 1
            , strokeColor = Nothing
            , cornerRadius = 10
            }
        )


viewScaledSvg : Layout ( String, node ) -> List (Html.Html msg) -> Html.Html msg
viewScaledSvg layout =
    Svg.svg
        [ SvgAttr.style
            ("position: absolute; pointer-events: none; left: 0; top: 0; width: "
                ++ String.fromInt layout.size.width
                ++ "px; height: "
                ++ String.fromInt layout.size.height
                ++ "px"
            )
        ]


viewLabels :
    String
    -> (node -> Bool -> Maybe (BranchLabel msg))
    -> List (Flow.Layout.ConditionRoot ( String, node ))
    -> Html.Html msg
viewLabels idPrefix viewBranchLabel roots =
    roots
        |> List.filterMap
            (\{ condition, branch, box } ->
                viewBranchLabel (Tuple.second condition) branch
                    |> Maybe.map
                        (\label ->
                            let
                                id : String
                                id =
                                    Tuple.first condition
                                        ++ branchLabelIdSuffix branch
                            in
                            ( id
                            , Html.div
                                [ Attr.style "translate" (String.fromInt box.x ++ "px " ++ String.fromInt (box.y - label.offsetY) ++ "px")
                                , Attr.style "position" "absolute"
                                , Attr.style "left" "0"
                                , Attr.style "top" "0"
                                , Attr.id (idPrefix ++ id)
                                ]
                                [ label.view ]
                            )
                        )
            )
        |> Html.Keyed.node "div" []


{-| Get the full id of a specific node to target it in a CSS stylesheet.
-}
cssNodeId : { viewerId : String, nodeId : String } -> String
cssNodeId { viewerId, nodeId } =
    viewerId ++ "-" ++ nodeId


{-| Get the full id of a branch label to target it in a CSS stylesheet
-}
cssBranchLabelId : { viewerId : String, nodeId : String, branch : Bool } -> String
cssBranchLabelId { viewerId, nodeId, branch } =
    cssNodeId { viewerId = viewerId, nodeId = nodeId } ++ branchLabelIdSuffix branch


branchLabelIdSuffix : Bool -> String
branchLabelIdSuffix branch =
    if branch then
        "-true-label"

    else
        "-false-label"


wheelEventDecoder : Json.Decode.Decoder ( Msg, Bool )
wheelEventDecoder =
    Json.Decode.map6
        (\deltaX deltaY ctrlKey metaKey clientX clientY ->
            ( if ctrlKey || metaKey then
                -- Ctrl key is true when pinching in and out in all major browsers.
                -- Safari supports non-standard gesture events, but other browsers do not.
                -- This also means that users without a trackpad can zoom by holding the Ctrl key.
                Pinched
                    { gapDelta = deltaY
                    , mouseX = clientX
                    , mouseY = clientY
                    }

              else
                Scrolled { deltaX = deltaX, deltaY = deltaY }
            , True
            )
        )
        (Json.Decode.field "deltaX" Json.Decode.float)
        (Json.Decode.field "deltaY" Json.Decode.float)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


panningEventDecoder : Json.Decode.Decoder Msg
panningEventDecoder =
    Json.Decode.map2 (\movementX movementY -> Panned { movementX = movementX, movementY = movementY })
        (Json.Decode.field "movementX" Json.Decode.float)
        (Json.Decode.field "movementY" Json.Decode.float)


mouseMoveEventDecoder : Json.Decode.Decoder ( Float, Float )
mouseMoveEventDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)

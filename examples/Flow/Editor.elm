module Flow.Editor exposing
    ( Model, Msg
    , init, update, subscriptions
    , editableLayout
    , Config, view
    , toMarkedFlow
    , Node(..), node, begin, middle, end, below
    , Emptiness(..), Terminality(..)
    , nodeToSpecId
    , hideAllButtons, showSingleButton, showButtonsNearPointer
    , AddOption, Edit(..), Reattaching, VisibleButtonPolicy
    )

{-|

@docs Model, Msg
@docs init, update, subscriptions
@docs editableLayout
@docs Config, view

@docs toMarkedFlow
@docs Node, node, begin, middle, end, below, terminal
@docs Emptiness, Terminality
@docs nodeToSpecId

@docs hideAllButtons, showSingleButton, showButtonsNearPointer

-}

import Browser.Events
import Flow exposing (Flow(..), Step(..))
import Flow.Layout exposing (Box, ConnectionEnding, Layout)
import Flow.Line
import Flow.Operation exposing (Operation)
import Flow.Viewer exposing (BranchLabel)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Html.Lazy
import Json.Decode
import Regex
import Svg
import Svg.Attributes as SvgAttr


type alias Model =
    { reattaching : Maybe Reattaching
    , openMenu : Maybe String
    , mouse : { x : Int, y : Int }
    , visibleButtonsSelector : String
    , visibleButtonPolicy : VisibleButtonPolicy
    }


type VisibleButtonPolicy
    = HideAll
    | ShowOnly String
    | ShowNearPointer


type alias Reattaching =
    { path : List Step
    , parentSeq : Maybe { index : Int, path : List Step }
    , box : Box
    }


init : Model
init =
    { reattaching = Nothing
    , openMenu = Nothing
    , mouse = { x = 0, y = 0 }
    , visibleButtonsSelector = ""
    , visibleButtonPolicy = ShowNearPointer
    }


type Msg kind
    = MouseMoved Int Int
    | VisibleButtonsUpdated String
    | AddMenuClicked String
    | AddBeforeClicked (List Step) kind
    | AddAfterClicked (List Step) kind
    | AddParallelClicked (List Step) kind
    | ClickedOutsideOpenMenu
    | ReattachStarted
        { path : List Step
        , box : Box
        , mouseX : Float
        , mouseY : Float
        }
    | ReattachCancelled
    | ReattachReleasedBeforeFirstTrueNode (List Step)
    | ReattachReleasedAfterTrueNode (List Step) Int
    | ReattachReleasedAfterCondition (List Step) Int


type Edit kind node
    = None
    | Added (List Step) kind ({ isCondition : Bool } -> node -> Operation node)
    | Operation (List Step) (Operation node)


update : Msg kind -> Flow.Viewer.Model (Node node) -> Model -> ( Model, Edit kind node )
update msg viewerModel model =
    case msg of
        MouseMoved x y ->
            ( { model | mouse = { x = x, y = y } }
            , None
            )

        VisibleButtonsUpdated newSelector ->
            ( { model
                | visibleButtonsSelector = newSelector
              }
            , None
            )

        AddMenuClicked nodeId ->
            ( { model
                | openMenu =
                    if model.openMenu == Just nodeId then
                        Nothing

                    else
                        Just nodeId
              }
            , None
            )

        AddBeforeClicked path kind ->
            ( { model | openMenu = Nothing }
            , Added (List.reverse path) kind Flow.Operation.AddBefore
            )

        AddAfterClicked path kind ->
            ( { model | openMenu = Nothing }
            , Added (List.reverse path) kind Flow.Operation.AddAfter
            )

        AddParallelClicked path kind ->
            ( { model | openMenu = Nothing }
            , Added (List.reverse path) kind Flow.Operation.AddParallelBelow
            )

        ClickedOutsideOpenMenu ->
            ( { model | openMenu = Nothing }, None )

        ReattachStarted { path, box, mouseX, mouseY } ->
            let
                parentSeq =
                    case path of
                        (Flow.InSequential seqIndex) :: rest ->
                            Just { index = seqIndex, path = rest }

                        (Flow.InConditionTrue seqIndex) :: rest ->
                            Just { index = seqIndex, path = rest }

                        (Flow.InConditionFalse seqIndex) :: rest ->
                            Just { index = seqIndex, path = rest }

                        _ ->
                            Nothing
            in
            ( { model
                | reattaching =
                    Just
                        { path = path
                        , parentSeq = parentSeq
                        , box = box
                        }
                , mouse = Flow.Viewer.toViewportCoords viewerModel mouseX mouseY
                , visibleButtonsSelector = ""
              }
            , None
            )

        ReattachCancelled ->
            ( { model
                | reattaching = Nothing
                , openMenu = Nothing
              }
            , None
            )

        ReattachReleasedBeforeFirstTrueNode path ->
            ( { model | reattaching = Nothing }
            , Flow.Operation.ReattachFalseInsideCondition { index = 0 }
                |> Operation (List.reverse path)
            )

        ReattachReleasedAfterTrueNode path index ->
            ( { model | reattaching = Nothing }
            , Flow.Operation.ReattachFalseInsideCondition { index = index + 1 }
                |> Operation (List.reverse path)
            )

        ReattachReleasedAfterCondition path index ->
            ( { model | reattaching = Nothing }
            , Flow.Operation.ReattachFalseOutsideCondition { index = index }
                |> Operation (List.reverse path)
            )


hideAllButtons : Model -> Model
hideAllButtons model =
    { model | visibleButtonPolicy = HideAll }


showSingleButton : String -> Model -> Model
showSingleButton id model =
    { model | visibleButtonPolicy = ShowOnly id }


showButtonsNearPointer : Model -> Model
showButtonsNearPointer model =
    { model | visibleButtonPolicy = ShowNearPointer }


subscriptions :
    { editorModel : Model
    , viewerModel : Flow.Viewer.Model (Node node)
    , toViewerMsg : Flow.Viewer.Msg -> msg
    , toEditorMsg : Msg kind -> msg
    }
    -> Sub msg
subscriptions { editorModel, viewerModel, toViewerMsg, toEditorMsg } =
    let
        onMouseMove x y =
            case editorModel.reattaching of
                Just _ ->
                    Json.Decode.succeed (toEditorMsg (MouseMoved x y))

                Nothing ->
                    let
                        newSelector : String
                        newSelector =
                            getVisibleButtonsSelector x y viewerModel.layout
                    in
                    if newSelector == editorModel.visibleButtonsSelector then
                        Json.Decode.fail "No change"

                    else
                        Json.Decode.succeed (toEditorMsg (VisibleButtonsUpdated newSelector))
    in
    Sub.batch
        [ Flow.Viewer.subscriptions
            viewerModel
            toViewerMsg
            (Just onMouseMove)
        , if editorModel.openMenu == Nothing then
            Sub.none

          else
            Browser.Events.onClick documentClickDecoder
                |> Sub.map toEditorMsg
        ]


documentClickDecoder : Json.Decode.Decoder (Msg kind)
documentClickDecoder =
    dataAttributePresentInAncestors menuDataAttributeName
        |> Json.Decode.andThen
            (\exists ->
                if exists then
                    Json.Decode.fail "Click inside menu. Ignore."

                else
                    Json.Decode.succeed ClickedOutsideOpenMenu
            )
        |> Json.Decode.field "target"


{-| Has some special handling to also work across elm-portal shenanigans with boundaries.
-}
dataAttributePresentInAncestors : String -> Json.Decode.Decoder Bool
dataAttributePresentInAncestors dataAttribute =
    Json.Decode.field "id" Json.Decode.string
        |> Json.Decode.andThen
            (\id ->
                Json.Decode.oneOf
                    [ Json.Decode.at [ "dataset", camelize dataAttribute ] (Json.Decode.succeed True)
                    , Json.Decode.field "parentElement" (Json.Decode.null ())
                        -- No more parents!
                        |> Json.Decode.map (\() -> False)
                    , Json.Decode.field "parentElement" (Json.Decode.lazy (\() -> dataAttributePresentInAncestors dataAttribute))
                    ]
            )


{-| Convert an underscored or dasherized string to a camelized one.

    camelize "-moz-transform" == "MozTransform"

-}
camelize : String -> String
camelize string =
    case Regex.fromString "[-_\\s]+(.)?" of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex
                (\{ submatches } ->
                    case submatches of
                        (Just match) :: _ ->
                            String.toUpper match

                        _ ->
                            ""
                )
                (String.trim string)


menuDataAttributeName : String
menuDataAttributeName =
    "flow-editor-menu"


menuDataAttribute : Html.Attribute msg
menuDataAttribute =
    Attr.attribute ("data-" ++ menuDataAttributeName) "1"


getVisibleButtonsSelector : Int -> Int -> Layout ( String, Node node ) -> String
getVisibleButtonsSelector x y layout =
    let
        nearMouse =
            List.filterMap
                (\( ( id, enode ), box ) ->
                    case enode of
                        Node _ ->
                            -- Ignore non-buttons
                            Nothing

                        _ ->
                            if
                                (abs (box.x + box.width // 2 - x) < nearButtonThreshold)
                                    && (abs (box.y + box.height // 2 - y) < nearButtonThreshold)
                            then
                                Just ("." ++ id)

                            else
                                Nothing
                )
                layout.nodes

        rightmost =
            List.filterMap
                (\( ending, _ ) ->
                    case ending of
                        Flow.Layout.CNode ( id, End _ _ _ ) ->
                            Just ("." ++ id)

                        _ ->
                            Nothing
                )
                layout.right
    in
    nearMouse
        ++ rightmost
        |> String.join ", "


nearButtonThreshold : number
nearButtonThreshold =
    300


type alias Config info kind node msg =
    { id : String
    , toEditorMsg : Msg kind -> msg
    , addOptions : List (AddOption kind)
    , toViewerMsg : Flow.Viewer.Msg -> msg
    , viewNode : info -> node -> Html.Html msg
    , viewRootNode : info -> Html.Html msg
    , viewBranchLabel : Node node -> Bool -> Maybe (BranchLabel msg)
    }


widthFill : Html.Attribute msg
widthFill =
    Attr.style "width" "100%"


widthPx : Int -> Html.Attribute msg
widthPx n =
    Attr.style "width" (String.fromInt n ++ "px")


heightFill : Html.Attribute msg
heightFill =
    Attr.style "height" "100%"


heightPx : Int -> Html.Attribute msg
heightPx n =
    Attr.style "height" (String.fromInt n ++ "px")


{-| -}
view :
    Config info kind node msg
    -> info
    -> Model
    -> Flow.Viewer.Model (Node node)
    -> Html.Html msg
view cfg info editorModel viewerModel =
    Html.div
        (if editorModel.reattaching == Nothing then
            [ widthFill
            , heightFill
            , Attr.class "flow-editor"
            ]

         else
            [ widthFill
            , heightFill
            , Attr.class "flow-editor flow-editor__reattaching"
            , Event.stopPropagationOn "mouseup"
                (Json.Decode.succeed
                    ( cfg.toEditorMsg ReattachCancelled
                    , True
                    )
                )
            ]
        )
        [ Flow.Viewer.view
            { id = cfg.id
            , viewNode =
                Flow.Viewer.Lazy4 viewNode
                    info
                    cfg
                    editorModel.openMenu
                    editorModel.reattaching
            , toMsg = cfg.toViewerMsg
            , model = viewerModel
            , viewBranchLabel = cfg.viewBranchLabel
            , viewConnection = Flow.Viewer.Lazy viewConnection editorModel.reattaching
            , scaledSvgExtra =
                case editorModel.reattaching of
                    Just p ->
                        let
                            line =
                                Flow.Line.Vertical
                                    { x = p.box.x
                                    , y = p.box.y + centerYOnConnection p.box + p.box.height // 2
                                    }
                                    editorModel.mouse
                                    |> Flow.Line.view []
                                        { stroke = Flow.Line.Dashed 5
                                        , strokeWidth = 2
                                        , strokeColor = Just "purple"
                                        , cornerRadius = 10
                                        }

                            point =
                                Svg.circle
                                    [ SvgAttr.cx (String.fromInt editorModel.mouse.x)
                                    , SvgAttr.cy (String.fromInt editorModel.mouse.y)
                                    , SvgAttr.r "9"
                                    , SvgAttr.style "fill: purple;"
                                    ]
                                    []
                        in
                        Just [ line, point ]

                    Nothing ->
                        Nothing
            }
        , Html.Lazy.lazy3 viewVisibleButtonsStylesheet
            editorModel.visibleButtonsSelector
            editorModel.visibleButtonPolicy
            editorModel.openMenu
        ]


viewVisibleButtonsStylesheet : String -> VisibleButtonPolicy -> Maybe String -> Html.Html msg
viewVisibleButtonsStylesheet visibleButtonsSelector visibleButtonPolicy maybeOpenMenu =
    let
        selector =
            case visibleButtonPolicy of
                HideAll ->
                    ""

                ShowOnly id ->
                    "." ++ id

                ShowNearPointer ->
                    maybeOpenMenu
                        |> Maybe.map (\id -> "." ++ id)
                        |> Maybe.withDefault visibleButtonsSelector
    in
    Html.node "style"
        []
        [ if String.isEmpty selector then
            Html.text ""

          else
            Html.text (selector ++ " { opacity: 1 }")
        ]


viewConnection :
    Maybe Reattaching
    -> ConnectionEnding ( String, Node node )
    -> ConnectionEnding ( String, Node node )
    -> Flow.Line.Path
    -> Html.Html msg
viewConnection reattaching a b =
    case ( a, b ) of
        -- Use dashed lines for add parallel buttons
        ( Flow.Layout.CNode ( id, Below _ ), _ ) ->
            viewAddParallelConnection id

        ( _, Flow.Layout.CNode ( id, Below _ ) ) ->
            viewAddParallelConnection id

        -- Hide connection when reattaching it
        ( Flow.Layout.CNode ( _, End ((InConditionFalse _) :: path) _ _ ), _ ) ->
            if isReattaching path reattaching then
                always (Html.text "")

            else
                Flow.Viewer.viewSolidConnection

        -- Draw reattaching connection to End node
        ( _, Flow.Layout.CNode ( _, End ((InConditionFalse index) :: path) _ _ ) ) ->
            if isReattaching path reattaching && index /= 0 then
                viewReattachSteppedConnection

            else
                Flow.Viewer.viewSolidConnection

        _ ->
            Flow.Viewer.viewSolidConnection


viewAddParallelConnection : String -> Flow.Line.Path -> Html.Html msg
viewAddParallelConnection =
    Html.Lazy.lazy2
        (\nodeId ->
            Flow.Line.view [ SvgAttr.class ("flow-editor__hidden " ++ nodeId) ]
                { stroke = Flow.Line.Dashed 5
                , strokeWidth = 1
                , strokeColor = Nothing
                , cornerRadius = 10
                }
        )


viewReattachSteppedConnection : Flow.Line.Path -> Html.Html msg
viewReattachSteppedConnection =
    Html.Lazy.lazy
        (Flow.Line.view []
            { stroke = Flow.Line.Dashed 5
            , strokeWidth = 2
            , strokeColor = Just "purple"
            , cornerRadius = 10
            }
        )


isReattaching : List Step -> Maybe Reattaching -> Bool
isReattaching path maybeReattaching =
    case maybeReattaching of
        Just reattaching ->
            reattaching.path == path

        Nothing ->
            False


type alias AddOption kind =
    { label : String
    , kind : kind
    }


viewNode :
    info
    -> Config info kind node msg
    -> Maybe String
    -> Maybe Reattaching
    -> ( String, Node node )
    -> Box
    -> Html.Html msg
viewNode info cfg openMenu reattaching ( nodeId, enode ) box =
    case enode of
        Root ->
            Html.Lazy.lazy cfg.viewRootNode info

        Node node_ ->
            Html.Lazy.lazy2 cfg.viewNode info node_

        Begin path ->
            viewWithReattachTarget
                { onReattach = \path_ _ -> ReattachReleasedBeforeFirstTrueNode path_
                , reattaching = reattaching
                , path = path
                , box = box
                , addMenu = viewAddMenu viewSmallAddButton (AddBeforeClicked path) cfg.addOptions openMenu box nodeId
                }
                |> Html.map cfg.toEditorMsg
                |> el
                    [ widthPx buttonSize
                    , heightFill
                    , Attr.style "justify-content" "center"
                    ]

        Middle path ->
            viewWithReattachTarget
                { onReattach = ReattachReleasedAfterTrueNode
                , reattaching = reattaching
                , path = path
                , box = box
                , addMenu = viewAddMenu viewSmallAddButton (AddAfterClicked path) cfg.addOptions openMenu box nodeId
                }
                |> Html.map cfg.toEditorMsg

        Below path ->
            viewAddMenu viewSmallAddButton (AddParallelClicked path) cfg.addOptions openMenu box nodeId
                |> Html.map cfg.toEditorMsg

        End (((InConditionFalse _) :: parentPath) as path) _ NonTerminal ->
            if isReattaching parentPath reattaching then
                Html.text ""

            else
                row []
                    [ viewAddMenu viewSmallAddButton (AddAfterClicked path) cfg.addOptions openMenu box nodeId
                        |> Html.map cfg.toEditorMsg
                        |> el
                            [ widthPx buttonSize
                            , heightPx buttonSize
                            , Attr.style "z-index" "2"
                            ]
                    , viewReattachHandle
                        { reattaching = reattaching
                        , parentPath = parentPath
                        , box = box
                        , hiddenUnlessMouseIsNearId = Just nodeId
                        }
                        |> Html.map cfg.toEditorMsg
                    ]

        End (((InConditionTrue _) :: _) as path) _ NonTerminal ->
            viewAddMenu viewSmallAddButton (AddAfterClicked path) cfg.addOptions openMenu box nodeId
                |> Html.map cfg.toEditorMsg
                |> el
                    [ widthPx buttonSize
                    , Attr.style "align-self" "center"
                    , heightFill
                    ]

        End path _ NonTerminal ->
            viewWithReattachTarget
                { onReattach = ReattachReleasedAfterTrueNode
                , reattaching = reattaching
                , path = path
                , box = box
                , addMenu = viewAddMenu viewSmallAddButton (AddAfterClicked path) cfg.addOptions openMenu box nodeId
                }
                |> Html.map cfg.toEditorMsg

        End (((InConditionFalse _) :: parentPath) as path) _ Terminal ->
            if isReattaching parentPath reattaching then
                Html.text ""

            else
                viewStandbyTerminalNode
                    { handle =
                        Just
                            (viewReattachHandle
                                { reattaching = reattaching
                                , parentPath = parentPath
                                , box = box
                                , hiddenUnlessMouseIsNearId = Nothing
                                }
                            )
                    , path = path
                    , addOptions = cfg.addOptions
                    , openMenu = openMenu
                    , box = box
                    , nodeId = nodeId
                    }
                    |> Html.map cfg.toEditorMsg

        End path _ Terminal ->
            viewWithReattachTarget
                { onReattach = ReattachReleasedAfterTrueNode
                , reattaching = reattaching
                , path = path
                , box = box
                , addMenu =
                    viewStandbyTerminalNode
                        { handle = Nothing
                        , path = path
                        , addOptions = cfg.addOptions
                        , openMenu = openMenu
                        , box = box
                        , nodeId = nodeId
                        }
                }
                |> Html.map cfg.toEditorMsg


viewAddMenu :
    (Bool -> Box -> String -> Html.Html (Msg kind))
    -> (kind -> Msg kind)
    -> List (AddOption kind)
    -> Maybe String
    -> Box
    -> String
    -> Html.Html (Msg kind)
viewAddMenu addButton onAdd addOptions openMenu box nodeId =
    if openMenu == Just nodeId then
        Html.div []
            [ addButton True box nodeId
            , addOptions
                |> List.map viewAddOption
                |> column
                    [ gap 8
                    , Attr.style "background-color" "white"
                    , Attr.style "padding" "8px"

                    -- We need a custom border-radius here so that it looks good
                    -- with respect to its parent
                    , Attr.style "border-radius" "7px"
                    , Attr.style "left" "100%"
                    , Attr.style "box-shadow" "0px 2px 4px rgba(0, 0, 0, 0.1)"
                    , Attr.style "width" "150px"
                    , Attr.style "transform" "translate(10px, 0)"
                    , menuDataAttribute
                    ]
                |> Html.map onAdd
            ]

    else
        addButton False box nodeId


viewAddOption : AddOption kind -> Html.Html kind
viewAddOption option =
    Html.div
        [ Attr.style "color" "rgba(0, 0, 0, 0.87)"
        , Attr.style "cursor" "pointer"
        ]
        [ row [ Attr.style "gap" "8px", Attr.style "background-color" "white" ]
            [ Html.span [ Attr.style "font-weight" "bold", Attr.style "font-size" "12px" ]
                [ Html.text option.label ]
            ]
        ]


el : List (Html.Attribute msg) -> Html.Html msg -> Html.Html msg
el attrs child =
    Html.div attrs [ child ]


row : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
row attrs children =
    Html.div
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "row"
            :: Attr.style "align-items" "center"
            :: attrs
        )
        children


column : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
column attrs children =
    Html.div
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "column"
            :: Attr.style "align-items" "center"
            :: attrs
        )
        children


gap : Int -> Html.Attribute msg
gap n =
    Attr.style "gap" (String.fromInt n ++ "px")


viewSmallAddButton : Bool -> Box -> String -> Html.Html (Msg kind)
viewSmallAddButton isOpen box nodeId =
    plusIcon
        |> el
            [ hiddenUnlessMouseIsNear nodeId
            , Attr.style "border-radius" "4px"
            , if isOpen then
                Attr.style "color" "#000000"

              else
                Attr.style "color" "#888888"
            , Attr.style "background-color" "#FFFFFF"
            , Attr.style "border-color"
                (if isOpen then
                    "#000000"

                 else
                    "#888888"
                )
            , Attr.style "border-width" "1px"
            , widthFill
            , heightFill
            , Attr.style "transform" ("translateY(" ++ String.fromInt (centerYOnConnection box) ++ "px)")
            , Attr.style "font-size" "12px"
            , Attr.class "flow-editor__add"
            , Event.onClick (AddMenuClicked nodeId)
            ]


hiddenUnlessMouseIsNear : String -> Html.Attribute msg
hiddenUnlessMouseIsNear nodeId =
    Attr.class ("flow-editor__hidden " ++ nodeId)


viewWithReattachTarget :
    { onReattach : List Step -> Int -> Msg kind
    , reattaching : Maybe Reattaching
    , path : List Step
    , box : Box
    , addMenu : Html.Html (Msg kind)
    }
    -> Html.Html (Msg kind)
viewWithReattachTarget { onReattach, reattaching, path, box, addMenu } =
    case reattaching |> Maybe.andThen (getReattachTargetMsg path onReattach) of
        Just msg ->
            el
                [ Attr.class "flow-editor__reattach-target"
                , Event.stopPropagationOn "mouseup" (Json.Decode.succeed ( msg, True ))
                ]
                (Html.text "")
                |> el
                    [ Attr.style "transform" ("translateY(" ++ String.fromInt (centerYOnConnection box) ++ "px)")
                    , Attr.style "width" (String.fromInt handleSize ++ "px")
                    , Attr.style "height" (String.fromInt handleSize ++ "px")
                    , widthPx handleSize
                    , heightPx handleSize
                    ]

        Nothing ->
            addMenu


getReattachTargetMsg :
    List Step
    -> (List Step -> Int -> Msg kind)
    -> Reattaching
    -> Maybe (Msg kind)
getReattachTargetMsg path onReattachAfterTrueNode reattaching =
    let
        maybeReattachAfterCondition index rest =
            case reattaching.parentSeq of
                Just parentSeq ->
                    if index > parentSeq.index && rest == parentSeq.path then
                        Just (ReattachReleasedAfterCondition reattaching.path index)

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    case path of
        (Flow.InConditionTrue index) :: rest ->
            if rest == reattaching.path then
                Just (onReattachAfterTrueNode reattaching.path index)

            else
                maybeReattachAfterCondition index rest

        (Flow.InSequential index) :: rest ->
            maybeReattachAfterCondition index rest

        (Flow.InConditionFalse index) :: rest ->
            maybeReattachAfterCondition index rest

        _ ->
            Nothing


viewReattachHandle :
    { reattaching : Maybe Reattaching
    , parentPath : List Step
    , box : Box
    , hiddenUnlessMouseIsNearId : Maybe String
    }
    -> Html.Html (Msg kind)
viewReattachHandle { reattaching, parentPath, box, hiddenUnlessMouseIsNearId } =
    let
        yTranslate =
            centerYOnConnection box
    in
    row []
        [ -- Line to handle
          el
            [ widthPx (buttonHandleSpacing + handleSize)
            , heightPx 1
            , Attr.style "background-color" "rgb(128, 128, 128)"
            , Attr.style "transform" ("translate(0px, " ++ String.fromInt yTranslate ++ "px)")
            , Attr.style "margin-left" (String.fromInt -handleSize ++ "px")
            ]
            (Html.text "")

        -- Handle
        , el
            [ widthPx handleSize
            , heightPx handleSize
            , Attr.style "border-radius" "50%"
            , Attr.style "cursor" "grab"
            , case reattaching of
                Just p ->
                    if p.path == parentPath then
                        Attr.style "background-color" "transparent"

                    else
                        Attr.style "background-color" "rgb(128, 128, 128)"

                Nothing ->
                    Attr.style "background-color" "rgb(128, 128, 128)"
            , case hiddenUnlessMouseIsNearId of
                Just id ->
                    hiddenUnlessMouseIsNear id

                Nothing ->
                    Attr.class ""
            , Attr.style "transform" ("translate(0px, " ++ String.fromInt yTranslate ++ "px)")
            , Event.stopPropagationOn "mousedown" <|
                Json.Decode.map2
                    (\x y ->
                        ( ReattachStarted
                            { path = parentPath
                            , box = box
                            , mouseX = x
                            , mouseY = y
                            }
                        , True
                        )
                    )
                    (Json.Decode.field "clientX" Json.Decode.float)
                    (Json.Decode.field "clientY" Json.Decode.float)
            ]
            (Html.text "")
        ]


viewStandbyTerminalNode :
    { handle : Maybe (Html.Html (Msg kind))
    , path : List Step
    , addOptions : List (AddOption kind)
    , openMenu : Maybe String
    , box : Box
    , nodeId : String
    }
    -> Html.Html (Msg kind)
viewStandbyTerminalNode { handle, path, addOptions, openMenu, box, nodeId } =
    let
        yTranslate =
            centerYOnConnection box
    in
    row []
        [ -- Add menu
          viewAddMenu viewBigAddButton (AddAfterClicked path) addOptions openMenu box nodeId
            |> el
                [ Attr.style "z-index" "2"
                , widthPx terminalButtonSize
                , heightPx terminalButtonSize
                ]

        -- Handle
        , case handle of
            Nothing ->
                Html.text ""

            Just h ->
                h

        -- Line before end marker
        , el
            [ widthPx beforeEndNodeSpacing
            , heightPx 1
            , Attr.style "background-color" "rgb(153, 153, 153)"
            , Attr.style "transform" ("translate(0, " ++ String.fromInt yTranslate ++ "px)")
            , Attr.style "z-index" "1"
            ]
            (Html.text "")

        -- End marker
        , el
            [ widthPx endNodeSize
            , heightPx endNodeSize
            , Attr.style "border-radius" "50%"
            , Attr.style "border-width" "2px"
            , Attr.style "border-color" "rgb(204, 204, 204)"
            , Attr.style "background-color" "rgb(238, 238, 238)"
            , Attr.style "box-shadow" "0px 2px 4px rgba(0, 0, 0, 0.1)"
            , Attr.style "display" "flex"
            , Attr.style "color" "rgb(51, 51, 51)"
            , Attr.style "transform" ("translate(0, " ++ String.fromInt yTranslate ++ "px)")
            ]
            (Html.span
                [ Attr.style "font-size" "12px"
                , Attr.style "font-weight" "bold"
                ]
                [ Html.text "End" ]
            )
        ]


plusIcon : Svg.Svg msg
plusIcon =
    Svg.svg
        [ SvgAttr.width "16"
        , SvgAttr.height "16"
        , SvgAttr.viewBox "0 0 16 16"
        , SvgAttr.fill "none"
        ]
        [ Svg.path
            [ SvgAttr.fillRule "evenodd"
            , SvgAttr.clipRule "evenodd"
            , SvgAttr.d "M8 1.67117C8.43753 1.67117 8.79222 2.02586 8.79222 2.46339L8.79222 7.20765L13.5365 7.20765C13.974 7.20765 14.3287 7.56234 14.3287 7.99987C14.3287 8.43741 13.974 8.7921 13.5365 8.7921L8.79222 8.7921L8.79222 13.5364C8.79222 13.9739 8.43753 14.3286 8 14.3286C7.56247 14.3286 7.20778 13.9739 7.20778 13.5364L7.20778 8.7921L2.46352 8.7921C2.02599 8.7921 1.6713 8.4374 1.6713 7.99987C1.6713 7.56234 2.02599 7.20765 2.46352 7.20765L7.20778 7.20765L7.20778 2.46339C7.20778 2.02586 7.56247 1.67117 8 1.67117Z"
            , SvgAttr.fill "#27272F"
            ]
            []
        ]


viewBigAddButton : Bool -> Box -> String -> Html.Html (Msg kind)
viewBigAddButton isOpen box nodeId =
    plusIcon
        |> el
            [ Attr.style "border-radius" "4px"
            , Attr.style "color" "var(--font-color-primary)"
            , Attr.style "background-color" "var(--background-color-white)"
            , Attr.style "border-color" "var(--border-color-primary)"
            , Attr.style "border-width" "1px"
            , Attr.style "width" (String.fromInt terminalButtonSize ++ "px")
            , Attr.style "height" (String.fromInt terminalButtonSize ++ "px")
            , Attr.style "transform" ("translate(0, " ++ String.fromInt (centerYOnConnection box) ++ "px)")
            , Attr.style "font-size" "var(--font-size-xs)"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            , Attr.class "flow-editor__add"
            , Event.onClick (AddMenuClicked nodeId)
            ]



-- Layout and properties


editableLayout :
    Flow.Layout.Pos
    ->
        { nodeProperties : node -> Flow.Layout.NodeProperties
        , rootProperties : Flow.Layout.NodeProperties
        , connectionY : Int
        , gapX : Int
        , gapY : Int
        , conditionBranchGapX : Int
        , conditionBranchGapY : Int
        }
    -> Flow node
    -> Layout (Node node)
editableLayout pos cfg flow =
    flow
        |> toMarkedFlow
        |> Flow.Layout.layout pos
            { nodeProperties = nodeProperties cfg
            , connectionY = connectionY cfg.connectionY
            , gapX = cfg.gapX
            , gapY = cfg.gapY
            , conditionBranchGapX = cfg.conditionBranchGapX + buttonSize * 2
            , conditionBranchGapY = cfg.conditionBranchGapY
            , isConditionRoot = isNode
            }


isNode : Node node -> Bool
isNode enode =
    case enode of
        Node _ ->
            True

        _ ->
            False


nodeProperties :
    { a
        | nodeProperties : node -> Flow.Layout.NodeProperties
        , rootProperties : Flow.Layout.NodeProperties
        , gapX : Int
        , conditionBranchGapX : Int
    }
    -> Node node
    -> Flow.Layout.NodeProperties
nodeProperties cfg enode =
    case enode of
        Root ->
            cfg.rootProperties

        Node n ->
            cfg.nodeProperties n

        Begin ((Flow.InConditionTrue _) :: _) ->
            buttonHandleProps

        Begin ((Flow.InConditionFalse _) :: _) ->
            buttonHandleProps

        End ((Flow.InConditionTrue _) :: _) emptiness NonTerminal ->
            emptyBranchEndNodeProps cfg emptiness

        End ((Flow.InConditionFalse _) :: _) emptiness NonTerminal ->
            emptyBranchEndNodeProps cfg emptiness

        End path emptiness Terminal ->
            let
                widthWithoutHandle : Int
                widthWithoutHandle =
                    terminalButtonSize
                        + beforeEndNodeSpacing
                        + endNodeSize
            in
            { width =
                case path of
                    (Flow.InConditionFalse _) :: _ ->
                        widthWithoutHandle
                            -- End node in False branch includes reattach handle
                            + buttonHandleSpacing
                            + handleSize

                    _ ->
                        widthWithoutHandle
            , height = terminalButtonSize
            , marginLeft =
                case emptiness of
                    NonEmpty ->
                        buttonSize + cfg.gapX

                    AllEmpty ->
                        buttonSize + cfg.gapX

                    Empty ->
                        -- Empty branch; add margin to align with the other branch
                        buttonHandleProps.width + cfg.gapX
            , marginRight = 0
            }

        _ ->
            singleButtonProps


emptyBranchEndNodeProps : { a | gapX : Int } -> Emptiness -> Flow.Layout.NodeProperties
emptyBranchEndNodeProps cfg emptiness =
    case emptiness of
        NonEmpty ->
            buttonHandleProps

        AllEmpty ->
            { buttonHandleProps
                | marginLeft = cfg.gapX
                , marginRight = cfg.gapX
            }

        Empty ->
            { buttonHandleProps | marginLeft = buttonHandleProps.width + cfg.gapX }


singleButtonProps : Flow.Layout.NodeProperties
singleButtonProps =
    { width = buttonSize
    , height = buttonSize
    , marginLeft = 0
    , marginRight = 0
    }


buttonHandleProps : Flow.Layout.NodeProperties
buttonHandleProps =
    { width = buttonSize + buttonHandleSpacing + handleSize
    , height = buttonSize
    , marginLeft = 0
    , marginRight = 0
    }


connectionY : Int -> ConnectionEnding (Node node) -> Int
connectionY y enode =
    case enode of
        Flow.Layout.CNode (Below _) ->
            buttonSize // 2

        _ ->
            y


centerYOnConnection : Box -> Int
centerYOnConnection box =
    box.connectionY - box.height // 2


buttonSize : number
buttonSize =
    18


terminalButtonSize : number
terminalButtonSize =
    32


buttonHandleSpacing : number
buttonHandleSpacing =
    16


handleSize : number
handleSize =
    16


beforeEndNodeSpacing : number
beforeEndNodeSpacing =
    22


endNodeSize : number
endNodeSize =
    40



-- Editable


{-| Represents key positions in the tree where buttons or reattach targets may be rendered.

  - Begin: Before the first node in a sequence if there is one
  - Middle: After each node in the sequence except the last one
  - End: After all nodes of a subsequence, but before another node in a parent sequence
  - Terminal: The end of the flow in a given branch
  - Below: Parallel under

Note: The path is reversed which simplifies writing custom logic for specific positions.

-}
type Node node
    = Root
    | Node node
    | Begin (List Step)
    | Middle (List Step)
    | End (List Step) Emptiness Terminality
    | Below (List Step)


type Emptiness
    = Empty
    | AllEmpty
    | NonEmpty


type Terminality
    = Terminal
    | NonTerminal


node : node -> Flow (Node node)
node =
    Flow.Node << Node


begin : List Step -> Flow (Node node)
begin =
    Flow.Node << Begin


middle : List Step -> Flow (Node node)
middle =
    Flow.Node << Middle


end : List Step -> Emptiness -> Terminality -> Flow (Node node)
end path emptiness terminality =
    Flow.Node (End path emptiness terminality)


below : List Step -> Flow (Node node)
below =
    Flow.Node << Below


toMarkedFlow : Flow node -> Flow (Node node)
toMarkedFlow flow =
    case flow of
        Flow.Node single ->
            Sequential
                [ Flow.Node Root
                , begin []
                , Parallel
                    [ node single
                    , below []
                    ]
                , end [] NonEmpty Terminal
                ]

        Flow.Parallel _ ->
            if Flow.isEmpty flow then
                Sequential
                    [ Flow.Node Root
                    , end [] Empty Terminal
                    ]

            else
                Sequential
                    [ Flow.Node Root
                    , begin []
                    , toMarkedFlowWith
                        { parent = Seq
                        , isLast = True
                        , path = []
                        }
                        flow
                    , end [] NonEmpty Terminal
                    ]
                    |> Flow.optimize

        Flow.Condition _ _ ->
            Sequential
                [ Flow.Node Root
                , begin []
                , toMarkedFlowWith
                    { parent = Seq
                    , isLast = True
                    , path = []
                    }
                    flow
                ]
                |> Flow.optimize

        _ ->
            Sequential
                [ Flow.Node Root
                , toMarkedFlowWith
                    { parent = Seq
                    , isLast = True
                    , path = []
                    }
                    flow
                ]
                |> Flow.optimize


type ParentCollection
    = Seq
    | Par


toMarkedFlowWith :
    { parent : ParentCollection
    , isLast : Bool
    , path : List Step
    }
    -> Flow node
    -> Flow (Node node)
toMarkedFlowWith { parent, isLast, path } flow =
    case flow of
        Flow.Node val ->
            node val
                |> wrapSingle
                    { parent = parent
                    , path = path
                    , isLast = isLast
                    }

        Sequential items ->
            if not (List.isEmpty items) then
                Sequential
                    (begin path
                        :: sequential
                            { isLast = isLast
                            , path = path
                            , tag = InSequential
                            }
                            items
                    )

            else if isLast then
                Sequential [ end path Empty Terminal ]

            else
                Sequential [ end path Empty NonTerminal ]

        Parallel items ->
            let
                prefix =
                    List.indexedMap (inParallel path) items
            in
            Parallel (prefix ++ [ below path ])

        Condition cond { trueSeq, falseSeq } ->
            Condition (Node cond)
                { trueSeq =
                    branch
                        { isLast = isLast
                        , parentPath = path
                        , tag = InConditionTrue
                        , oppositeBranchIsEmpty = List.isEmpty falseSeq
                        }
                        trueSeq
                , falseSeq =
                    branch
                        { isLast = isLast
                        , parentPath = path
                        , tag = InConditionFalse
                        , oppositeBranchIsEmpty = List.isEmpty trueSeq
                        }
                        falseSeq
                }
                |> wrapSingle
                    { parent = parent
                    , path = path
                    , isLast = isLast
                    }


wrapSingle : { parent : ParentCollection, isLast : Bool, path : List Step } -> Flow (Node node) -> Flow (Node node)
wrapSingle { parent, isLast, path } single =
    case parent of
        Seq ->
            Parallel
                [ single
                , below path
                ]

        Par ->
            Sequential
                [ begin path
                , single
                , if isLast then
                    end path NonEmpty Terminal

                  else
                    end path NonEmpty NonTerminal
                ]


branch :
    { isLast : Bool
    , parentPath : List Step
    , tag : Int -> Step
    , oppositeBranchIsEmpty : Bool
    }
    -> List (Flow node)
    -> List (Flow (Node node))
branch { isLast, parentPath, tag, oppositeBranchIsEmpty } items =
    let
        path =
            tag 0 :: parentPath
    in
    if not (List.isEmpty items) then
        begin path
            :: sequential
                { isLast = isLast
                , path = parentPath
                , tag = tag
                }
                items

    else
        [ end path
            (if oppositeBranchIsEmpty then
                AllEmpty

             else
                Empty
            )
            (if isLast then
                Terminal

             else
                NonTerminal
            )
        ]


sequential :
    { isLast : Bool
    , path : List Step
    , tag : Int -> Step
    }
    -> List (Flow node)
    -> List (Flow (Node node))
sequential { isLast, path, tag } items =
    let
        total =
            List.length items
    in
    items
        |> List.indexedMap
            (inSequential
                { parentIsLast = isLast
                , parentPath = path
                , tag = tag
                , total = total
                }
            )
        |> List.concat


inSequential :
    { parentIsLast : Bool
    , parentPath : List Step
    , tag : Int -> Step
    , total : Int
    }
    -> Int
    -> Flow node
    -> List (Flow (Node node))
inSequential { parentIsLast, parentPath, tag, total } index item =
    let
        path =
            tag index :: parentPath

        editable isLast =
            toMarkedFlowWith
                { parent = Seq
                , isLast = isLast
                , path = path
                }
                item
    in
    if index + 1 == total then
        if Flow.isCondition item then
            [ editable parentIsLast ]

        else if parentIsLast then
            [ editable False, end path NonEmpty Terminal ]

        else
            [ editable False, end path NonEmpty NonTerminal ]

    else
        [ editable False, middle path ]


inParallel : List Step -> Int -> Flow node -> Flow (Node node)
inParallel parentPath index =
    toMarkedFlowWith
        { parent = Par
        , isLast = False
        , path = InParallel index :: parentPath
        }


nodeToSpecId : (node -> String) -> Node node -> String
nodeToSpecId toId enode =
    case enode of
        Root ->
            rootId

        Node val ->
            toId val

        Begin path ->
            opToId "begin" path

        Middle path ->
            opToId "middle" path

        End path _ _ ->
            opToId "end" path

        Below path ->
            opToId "below" path


rootId : String
rootId =
    "root"


opToId : String -> List Step -> String
opToId tag path =
    let
        pathStr =
            path
                |> List.map stepToId
                |> String.concat
    in
    tag ++ "-" ++ pathStr


stepToId : Step -> String
stepToId step =
    let
        toStr tag index =
            tag ++ String.fromInt index
    in
    case step of
        InSequential index ->
            toStr "s" index

        InParallel index ->
            toStr "p" index

        InConditionTrue index ->
            toStr "t" index

        InConditionFalse index ->
            toStr "f" index

module Editor exposing (main)

{-| -}

import Browser
import Flow exposing (Flow(..))
import Flow.Editor
import Flow.Layout exposing (Layout)
import Flow.Operation
import Flow.Viewer
import Html
import Html.Attributes as Attr
import Html.Events as Event


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { flow : Flow String
    , viewer : Flow.Viewer.Model (Flow.Editor.Node String)
    , editor : Flow.Editor.Model
    , sequence : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        flow =
            Sequential
                [ Node "A"
                , Condition "?"
                    { trueSeq =
                        [ Node "T1"
                        , Parallel
                            [ Node "P1"
                            , Node "P2"
                            ]
                        , Node "T2"
                        ]
                    , falseSeq =
                        [ Node "F1"
                        , Node "F2"
                        ]
                    }

                -- , Node "E"
                ]
    in
    ( { flow = flow
      , viewer =
            flow
                |> layoutEditor
                |> Flow.Viewer.init { nodeToString = nodeToString }
      , editor = Flow.Editor.init
      , sequence = 0
      }
    , Cmd.none
    )


layoutEditor : Flow node -> Layout (Flow.Editor.Node node)
layoutEditor =
    Flow.Editor.editableLayout { x = 0, y = 0 }
        { nodeProperties = always { width = 80, height = 30, marginLeft = 0, marginRight = 0 }
        , rootProperties = { width = 80, height = 30, marginLeft = 0, marginRight = 0 }
        , connectionY = 15
        , gapX = 30
        , gapY = 20
        , conditionBranchGapX = 30
        , conditionBranchGapY = 60
        }


nodeToString : Flow.Editor.Node String -> String
nodeToString =
    Flow.Editor.nodeToSpecId identity



-- UPDATE


type NodeKind
    = Step
    | Cond


type Msg
    = ViewerMsg Flow.Viewer.Msg
    | EditorMsg (Flow.Editor.Msg NodeKind)
    | SeeAllClicked
    | NodeClicked String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewerMsg viewerMsg ->
            ( { model
                | viewer = Flow.Viewer.update viewerMsg model.viewer
              }
            , Cmd.none
            )

        EditorMsg editorMsg ->
            let
                ( newEditor, edit ) =
                    Flow.Editor.update editorMsg model.viewer model.editor

                applyOperation path op =
                    let
                        newFlow =
                            model.flow
                                |> Flow.Operation.applyAt { nodeToId = identity } path op
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault model.flow
                                |> Flow.optimize

                        newViewer =
                            model.viewer
                                |> Flow.Viewer.setLayout
                                    { nodeToString = nodeToString
                                    , layout = layoutEditor newFlow
                                    }
                    in
                    ( newFlow, newViewer )
            in
            case edit of
                Flow.Editor.None ->
                    ( { model | editor = newEditor }
                    , Cmd.none
                    )

                Flow.Editor.Added path kind toOp ->
                    let
                        newSeq =
                            model.sequence + 1

                        newNode =
                            String.fromInt newSeq

                        ( newFlow, newViewer ) =
                            newNode
                                |> toOp { isCondition = kind == Cond }
                                |> applyOperation path
                    in
                    ( { model
                        | flow = newFlow
                        , viewer =
                            newViewer
                                |> Flow.Viewer.focusOn { id = newNode }
                        , editor = newEditor
                        , sequence = newSeq
                      }
                    , Cmd.none
                    )

                Flow.Editor.Operation path operation ->
                    let
                        ( newFlow, newViewer ) =
                            applyOperation path operation
                    in
                    ( { model
                        | flow = newFlow
                        , viewer = newViewer
                        , editor = newEditor
                      }
                    , Cmd.none
                    )

        SeeAllClicked ->
            ( { model
                | viewer =
                    model.viewer
                        |> Flow.Viewer.seeAll
                        |> Flow.Viewer.withTransitions
              }
            , Cmd.none
            )

        NodeClicked id ->
            ( { model
                | viewer =
                    model.viewer
                        |> Flow.Viewer.focusOn { id = id }
                        |> Flow.Viewer.withTransitions
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Flow.Editor.subscriptions
        { editorModel = model.editor
        , toEditorMsg = EditorMsg
        , viewerModel = model.viewer
        , toViewerMsg = ViewerMsg
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Workflow Editor"
    , body =
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            , Attr.style "width" "100vw"
            , Attr.style "height" "100vh"
            ]
            [ Flow.Editor.view editorConfig () model.editor model.viewer
            , Html.div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "row"
                , Attr.style "justify-content" "center"
                , Attr.style "position" "fixed"
                , Attr.style "bottom" "20px"
                , Attr.style "left" "20px"
                ]
                [ Html.button [ Event.onClick SeeAllClicked ]
                    [ Html.text "See all workflow"
                    ]
                , Html.span [ Attr.style "font-size" "16px" ] [ Html.text "Click nodes to center on them!" ]
                ]
            ]
        ]
    }


editorConfig : Flow.Editor.Config () NodeKind String Msg
editorConfig =
    { id = "sb-flow"
    , toEditorMsg = EditorMsg
    , addOptions = addOptions
    , toViewerMsg = ViewerMsg
    , viewNode = viewNode
    , viewRootNode = viewRootNode
    , viewBranchLabel = viewBranchLabel
    }


viewNode : () -> String -> Html.Html Msg
viewNode () node =
    Html.div
        [ Attr.style "background-color" "grey"
        , Attr.style "color" "white"
        , Attr.style "border-radius" "4px"
        , Attr.style "width" "100%"
        , Attr.style "padding" "8px 4px"
        , Event.onClick (NodeClicked node)
        ]
        [ Html.text node ]


viewRootNode : () -> Html.Html Msg
viewRootNode _ =
    viewNode () "R"


addOptions : List (Flow.Editor.AddOption NodeKind)
addOptions =
    [ { label = "Step"
      , kind = Step
      }
    , { label = "Condition"
      , kind = Cond
      }
    ]


viewBranchLabel : Flow.Editor.Node String -> Bool -> Maybe (Flow.Viewer.BranchLabel Msg)
viewBranchLabel _ branch =
    Just
        { offsetY = 25
        , view =
            Html.span
                [ Attr.style "color" "grey" ]
                [ Html.text <|
                    if branch then
                        "True"

                    else
                        "False"
                ]
        }

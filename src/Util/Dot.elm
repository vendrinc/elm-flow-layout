module Util.Dot exposing (Vertex, Edge, toDot)

{-| Graphviz DOT language.

There is an online DOT viewer: <https://dreampuf.github.io/GraphvizOnline/>

@docs Vertex, Edge, toDot

-}

import Array


type alias Vertex =
    { id : String
    , label : String
    , bgcolor : Maybe String -- takes HTML named colors like "pink"
    }


type alias Edge =
    { from : String
    , to : String
    , label : Maybe String
    }


toDot : List Vertex -> List Edge -> String
toDot unescapedVertices unescapedEdges =
    let
        vertices : List Vertex
        vertices =
            unescapedVertices
                |> List.map
                    (\v ->
                        { id = escape v.id
                        , label = escape v.label
                        , bgcolor = Maybe.map escape v.bgcolor
                        }
                    )

        edges : List Edge
        edges =
            unescapedEdges
                |> List.map
                    (\e ->
                        { from = escape e.from
                        , to = escape e.to
                        , label = Maybe.map escape e.label
                        }
                    )
    in
    """
digraph G {

    rankdir=LR
    node [shape="box"]

{VERTICES}

{EDGES}

}
    """
        |> String.replace "{VERTICES}"
            (vertices
                |> List.map viewVertex
                |> String.join "\n"
            )
        |> String.replace "{EDGES}"
            (edges
                |> List.indexedMap viewEdge
                |> String.join "\n"
            )


viewVertex : Vertex -> String
viewVertex v =
    let
        attrs =
            viewAttrs
                [ if v.id /= v.label then
                    Just
                        [ ( "label", v.label )
                        ]

                  else
                    Nothing
                , v.bgcolor
                    |> Maybe.map
                        (\color ->
                            [ ( "fillcolor", color )
                            , ( "style", "filled" )
                            ]
                        )
                ]
    in
    "    \"{ID}\" {ATTRS}"
        |> String.replace "{ID}" v.id
        |> String.replace "{LABEL}" v.label
        |> String.replace "{ATTRS}" attrs


viewEdge : Int -> Edge -> String
viewEdge index v =
    let
        color =
            colors
                |> Array.get (index |> modBy colorsLen)
                |> Maybe.withDefault "gray0"

        attrs =
            viewAttrs
                [ v.label |> Maybe.map (\label -> [ ( "label", label ) ])
                , Just [ ( "color", color ) ]
                ]
    in
    "    \"{FROM}\" -> \"{TO}\"{ATTRS}"
        |> String.replace "{FROM}" v.from
        |> String.replace "{TO}" v.to
        |> String.replace "{ATTRS}" attrs


viewAttrs : List (Maybe (List ( String, String ))) -> String
viewAttrs attrs =
    let
        filtered =
            attrs
                |> List.filterMap identity
                |> List.concat
                |> List.map (\( name, val ) -> name ++ "=\"" ++ val ++ "\"")
    in
    if List.isEmpty filtered then
        ""

    else
        "[" ++ String.join "," filtered ++ "]"


colors : Array.Array String
colors =
    Array.fromList
        [ "blue"
        , "blueviolet"
        , "brown"
        , "chocolate"
        , "cornflowerblue"
        , "darkblue"
        , "darkorchid"
        , "darkred"
        , "deeppink"
        , "dodgerblue4"
        , "fuchsia"
        , "gold4"
        , "gray0"
        , "green4"
        , "indigo"
        , "midnightblue"
        , "purple"
        , "violetred"
        ]


colorsLen : Int
colorsLen =
    Array.length colors


escape : String -> String
escape str =
    str
        |> String.replace "\"" "\\\""

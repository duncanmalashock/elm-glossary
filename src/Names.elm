module Names exposing
    ( Names, fromFile
    , toJson
    )

{-|

@docs Names, fromFile
@docs toJson

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Json.Encode


fromFile : Elm.Syntax.File.File -> Names
fromFile file =
    List.foldl accumulateNamesFromDeclaration new file.declarations


toJson : Names -> Json.Encode.Value
toJson names =
    Json.Encode.object
        [ ( "functions", Json.Encode.list Json.Encode.string names.functions )
        , ( "functionArguments", Json.Encode.list Json.Encode.string names.functionArguments )
        ]


type alias Names =
    { functions : List String
    , functionArguments : List String
    }


new : Names
new =
    { functions = []
    , functionArguments = []
    }


accumulateNamesFromDeclaration :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> Names
    -> Names
accumulateNamesFromDeclaration (Elm.Syntax.Node.Node _ declaration) current =
    case declaration of
        FunctionDeclaration fn ->
            let
                functionImplementation : Elm.Syntax.Expression.FunctionImplementation
                functionImplementation =
                    Elm.Syntax.Node.value fn.declaration

                name : String
                name =
                    Elm.Syntax.Node.value functionImplementation.name

                arguments : List String
                arguments =
                    functionImplementation.arguments
                        |> List.map Elm.Syntax.Node.value
                        |> List.concatMap patternToNames

                --FunctionImplementation =
                --    { name : Node String
                --    , arguments : List (Node Pattern)
                --    , expression : Node Expression
                --    }
            in
            { current
                | functions = name :: current.functions
                , functionArguments = arguments ++ current.functionArguments
            }

        AliasDeclaration _ ->
            current

        CustomTypeDeclaration _ ->
            current

        PortDeclaration _ ->
            current

        InfixDeclaration _ ->
            current

        Destructuring _ _ ->
            current


patternToNames : Elm.Syntax.Pattern.Pattern -> List String
patternToNames pattern =
    case pattern of
        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern char ->
            []

        Elm.Syntax.Pattern.StringPattern string ->
            []

        Elm.Syntax.Pattern.IntPattern int ->
            []

        Elm.Syntax.Pattern.HexPattern int ->
            []

        Elm.Syntax.Pattern.FloatPattern float ->
            []

        Elm.Syntax.Pattern.TuplePattern nodes ->
            List.map Elm.Syntax.Node.value nodes
                |> List.concatMap patternToNames

        Elm.Syntax.Pattern.RecordPattern nodes ->
            List.map Elm.Syntax.Node.value nodes

        Elm.Syntax.Pattern.UnConsPattern node1 node2 ->
            List.map Elm.Syntax.Node.value [ node1, node2 ]
                |> List.concatMap patternToNames

        Elm.Syntax.Pattern.ListPattern nodes ->
            List.map Elm.Syntax.Node.value nodes
                |> List.concatMap patternToNames

        Elm.Syntax.Pattern.VarPattern string ->
            [ string ]

        Elm.Syntax.Pattern.NamedPattern qualifiedNameRef nodes ->
            []

        Elm.Syntax.Pattern.AsPattern node nameNode ->
            Elm.Syntax.Node.value nameNode
                |> List.singleton

        Elm.Syntax.Pattern.ParenthesizedPattern node ->
            []

module Names exposing
    ( Names, fromFile
    , toJson
    )

{-|

@docs Names, fromFile
@docs toJson

-}

import Dict
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Json.Encode


fromFile : Elm.Syntax.File.File -> Names
fromFile file =
    List.map Elm.Syntax.Node.value file.declarations
        |> List.foldl accumulateNamesFromDeclaration new


toJson : Names -> Json.Encode.Value
toJson names =
    let
        countDictToObject : Dict.Dict String Int -> Json.Encode.Value
        countDictToObject countDict =
            Dict.toList countDict
                |> List.sort
                |> List.map (\( name, count ) -> ( name, Json.Encode.int count ))
                |> Json.Encode.object
    in
    Json.Encode.object
        [ ( "functions", countDictToObject names.functions )
        , ( "functionArguments", countDictToObject names.functionArguments )
        ]


type alias Names =
    { functions : Dict.Dict String Int
    , functionArguments : Dict.Dict String Int
    }


new : Names
new =
    { functions = Dict.empty
    , functionArguments = Dict.empty
    }


updateCount : Maybe Int -> Maybe Int
updateCount maybeInt =
    case maybeInt of
        Just int ->
            Just (int + 1)

        Nothing ->
            Just 1


addNameCount : String -> Dict.Dict String Int -> Dict.Dict String Int
addNameCount newName currentDict =
    Dict.update newName updateCount currentDict


addNameCounts : List String -> Dict.Dict String Int -> Dict.Dict String Int
addNameCounts newNames currentDict =
    List.foldl (\name -> Dict.update name updateCount) currentDict newNames


accumulateNamesFromExpression :
    Elm.Syntax.Expression.Expression
    -> Names
    -> Names
accumulateNamesFromExpression expression current =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            current

        Elm.Syntax.Expression.Application nodes ->
            current

        Elm.Syntax.Expression.OperatorApplication string infixDirection node1 node2 ->
            current

        Elm.Syntax.Expression.FunctionOrValue moduleName functionName ->
            current

        Elm.Syntax.Expression.IfBlock node node1 node2 ->
            current

        Elm.Syntax.Expression.PrefixOperator string ->
            current

        Elm.Syntax.Expression.Operator string ->
            current

        Elm.Syntax.Expression.Integer int ->
            current

        Elm.Syntax.Expression.Hex int ->
            current

        Elm.Syntax.Expression.Floatable float ->
            current

        Elm.Syntax.Expression.Negation node ->
            current

        Elm.Syntax.Expression.Literal string ->
            current

        Elm.Syntax.Expression.CharLiteral char ->
            current

        Elm.Syntax.Expression.TupledExpression nodes ->
            current

        Elm.Syntax.Expression.ParenthesizedExpression node ->
            current

        Elm.Syntax.Expression.LetExpression letBlock ->
            let
                letDeclarations : List Elm.Syntax.Expression.LetDeclaration
                letDeclarations =
                    List.map Elm.Syntax.Node.value letBlock.declarations

                letExpression : Elm.Syntax.Expression.Expression
                letExpression =
                    Elm.Syntax.Node.value letBlock.expression

                updatedNames =
                    current
                        |> accumulateNamesFromExpression letExpression
            in
            List.foldl accumulateNamesFromLetDeclaration updatedNames letDeclarations

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            current

        Elm.Syntax.Expression.LambdaExpression lambda ->
            current

        Elm.Syntax.Expression.RecordExpr nodes ->
            current

        Elm.Syntax.Expression.ListExpr nodes ->
            current

        Elm.Syntax.Expression.RecordAccess node1 node2 ->
            current

        Elm.Syntax.Expression.RecordAccessFunction string ->
            current

        Elm.Syntax.Expression.RecordUpdateExpression node nodes ->
            current

        Elm.Syntax.Expression.GLSLExpression string ->
            current


accumulateNamesFromLetDeclaration :
    Elm.Syntax.Expression.LetDeclaration
    -> Names
    -> Names
accumulateNamesFromLetDeclaration letDeclaration current =
    case letDeclaration of
        Elm.Syntax.Expression.LetFunction fn ->
            let
                functionImplementation : Elm.Syntax.Expression.FunctionImplementation
                functionImplementation =
                    Elm.Syntax.Node.value fn.declaration
            in
            accumulateNamesFromFunctionImplementation functionImplementation current

        Elm.Syntax.Expression.LetDestructuring patternNode expressionNode ->
            let
                arguments : List String
                arguments =
                    patternToNames (Elm.Syntax.Node.value patternNode)

                expression : Elm.Syntax.Expression.Expression
                expression =
                    Elm.Syntax.Node.value expressionNode
            in
            { current
                | functionArguments =
                    addNameCounts arguments current.functions
            }
                |> accumulateNamesFromExpression expression


accumulateNamesFromFunctionImplementation :
    Elm.Syntax.Expression.FunctionImplementation
    -> Names
    -> Names
accumulateNamesFromFunctionImplementation functionImplementation current =
    let
        functionName : String
        functionName =
            Elm.Syntax.Node.value functionImplementation.name

        arguments : List String
        arguments =
            functionImplementation.arguments
                |> List.map Elm.Syntax.Node.value
                |> List.concatMap patternToNames

        updatedNames : Names
        updatedNames =
            { current
                | functions = addNameCount functionName current.functions
                , functionArguments = addNameCounts arguments current.functionArguments
            }

        expression : Elm.Syntax.Expression.Expression
        expression =
            Elm.Syntax.Node.value functionImplementation.expression
    in
    accumulateNamesFromExpression expression updatedNames


accumulateNamesFromDeclaration :
    Elm.Syntax.Declaration.Declaration
    -> Names
    -> Names
accumulateNamesFromDeclaration declaration current =
    case declaration of
        FunctionDeclaration fn ->
            let
                functionImplementation : Elm.Syntax.Expression.FunctionImplementation
                functionImplementation =
                    Elm.Syntax.Node.value fn.declaration
            in
            accumulateNamesFromFunctionImplementation functionImplementation current

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
            []

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

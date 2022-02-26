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


type alias Names =
    { functions : Dict.Dict String Int
    , functionArguments : Dict.Dict String Int
    , types : Dict.Dict String Int
    , typeVariants : Dict.Dict String Int
    , typeAliases : Dict.Dict String Int
    , typeVariables : Dict.Dict String Int
    }


new : Names
new =
    { functions = Dict.empty
    , functionArguments = Dict.empty
    , types = Dict.empty
    , typeVariants = Dict.empty
    , typeAliases = Dict.empty
    , typeVariables = Dict.empty
    }


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

        toObjectFromKeyValues : List ( String, Dict.Dict String Int ) -> Json.Encode.Value
        toObjectFromKeyValues keyValues =
            keyValues
                |> List.map
                    (\( key, dict ) ->
                        ( key, countDictToObject dict )
                    )
                |> Json.Encode.object
    in
    toObjectFromKeyValues
        [ ( "functions", names.functions )
        , ( "functionArguments", names.functionArguments )
        , ( "types", names.types )
        , ( "typeVariants", names.typeVariants )
        , ( "typeAliases", names.typeAliases )
        , ( "typeVariables", names.typeVariables )
        ]


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
accumulateNamesFromExpression expression names =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            names

        Elm.Syntax.Expression.Application nodes ->
            names

        Elm.Syntax.Expression.OperatorApplication string infixDirection node1 node2 ->
            names

        Elm.Syntax.Expression.FunctionOrValue moduleName functionName ->
            names

        Elm.Syntax.Expression.IfBlock node node1 node2 ->
            names

        Elm.Syntax.Expression.PrefixOperator string ->
            names

        Elm.Syntax.Expression.Operator string ->
            names

        Elm.Syntax.Expression.Integer int ->
            names

        Elm.Syntax.Expression.Hex int ->
            names

        Elm.Syntax.Expression.Floatable float ->
            names

        Elm.Syntax.Expression.Negation node ->
            names

        Elm.Syntax.Expression.Literal string ->
            names

        Elm.Syntax.Expression.CharLiteral char ->
            names

        Elm.Syntax.Expression.TupledExpression nodes ->
            names

        Elm.Syntax.Expression.ParenthesizedExpression node ->
            names

        Elm.Syntax.Expression.LetExpression letBlock ->
            let
                letDeclarations : List Elm.Syntax.Expression.LetDeclaration
                letDeclarations =
                    List.map Elm.Syntax.Node.value letBlock.declarations

                letExpression : Elm.Syntax.Expression.Expression
                letExpression =
                    Elm.Syntax.Node.value letBlock.expression

                updatedNames =
                    names
                        |> accumulateNamesFromExpression letExpression
            in
            List.foldl accumulateNamesFromLetDeclaration updatedNames letDeclarations

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            names

        Elm.Syntax.Expression.LambdaExpression lambda ->
            names

        Elm.Syntax.Expression.RecordExpr nodes ->
            names

        Elm.Syntax.Expression.ListExpr nodes ->
            names

        Elm.Syntax.Expression.RecordAccess node1 node2 ->
            names

        Elm.Syntax.Expression.RecordAccessFunction string ->
            names

        Elm.Syntax.Expression.RecordUpdateExpression node nodes ->
            names

        Elm.Syntax.Expression.GLSLExpression string ->
            names


accumulateNamesFromLetDeclaration :
    Elm.Syntax.Expression.LetDeclaration
    -> Names
    -> Names
accumulateNamesFromLetDeclaration letDeclaration names =
    case letDeclaration of
        Elm.Syntax.Expression.LetFunction fn ->
            let
                functionImplementation : Elm.Syntax.Expression.FunctionImplementation
                functionImplementation =
                    Elm.Syntax.Node.value fn.declaration
            in
            accumulateNamesFromFunctionImplementation functionImplementation names

        Elm.Syntax.Expression.LetDestructuring patternNode expressionNode ->
            let
                arguments : List String
                arguments =
                    patternToNames (Elm.Syntax.Node.value patternNode)

                expression : Elm.Syntax.Expression.Expression
                expression =
                    Elm.Syntax.Node.value expressionNode
            in
            { names
                | functionArguments =
                    addNameCounts arguments names.functions
            }
                |> accumulateNamesFromExpression expression


accumulateNamesFromFunctionImplementation :
    Elm.Syntax.Expression.FunctionImplementation
    -> Names
    -> Names
accumulateNamesFromFunctionImplementation functionImplementation names =
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
            { names
                | functions = addNameCount functionName names.functions
                , functionArguments = addNameCounts arguments names.functionArguments
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
accumulateNamesFromDeclaration declaration names =
    case declaration of
        FunctionDeclaration fn ->
            let
                functionImplementation : Elm.Syntax.Expression.FunctionImplementation
                functionImplementation =
                    Elm.Syntax.Node.value fn.declaration
            in
            accumulateNamesFromFunctionImplementation functionImplementation names

        AliasDeclaration aliasDeclaration ->
            let
                name : String
                name =
                    Elm.Syntax.Node.value aliasDeclaration.name

                typeVariableNames : List String
                typeVariableNames =
                    List.map Elm.Syntax.Node.value aliasDeclaration.generics
            in
            { names
                | typeAliases =
                    addNameCount name names.typeAliases
                , typeVariables =
                    addNameCounts typeVariableNames names.typeVariables
            }

        CustomTypeDeclaration typeDeclaration ->
            let
                name : String
                name =
                    Elm.Syntax.Node.value typeDeclaration.name

                variantNames : List String
                variantNames =
                    List.map Elm.Syntax.Node.value typeDeclaration.constructors
                        |> List.map (.name >> Elm.Syntax.Node.value)

                typeVariableNames : List String
                typeVariableNames =
                    List.map Elm.Syntax.Node.value typeDeclaration.generics
            in
            { names
                | types =
                    addNameCount name names.types
                , typeVariants =
                    addNameCounts variantNames names.typeVariants
                , typeVariables =
                    addNameCounts typeVariableNames names.typeVariables
            }

        PortDeclaration _ ->
            names

        InfixDeclaration _ ->
            names

        Destructuring _ _ ->
            names


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

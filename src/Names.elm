module Names exposing (Names, fromFilesToJson)

{-|

@docs Names, fromFilesToJson

-}

import Dict
import Elm.Parser
import Elm.Processing
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
    , ports : Dict.Dict String Int
    }


empty : Names
empty =
    { functions = Dict.empty
    , functionArguments = Dict.empty
    , types = Dict.empty
    , typeVariants = Dict.empty
    , typeAliases = Dict.empty
    , typeVariables = Dict.empty
    , ports = Dict.empty
    }


combine : Names -> Names -> Names
combine names1 names2 =
    { functions = combineDict names1.functions names2.functions
    , functionArguments = combineDict names1.functionArguments names2.functionArguments
    , types = combineDict names1.types names2.types
    , typeVariants = combineDict names1.typeVariants names2.typeVariants
    , typeAliases = combineDict names1.typeAliases names2.typeAliases
    , typeVariables = combineDict names1.typeVariables names2.typeVariables
    , ports = combineDict names1.ports names2.ports
    }


combineDict : Dict.Dict String Int -> Dict.Dict String Int -> Dict.Dict String Int
combineDict dict1 dict2 =
    List.foldl (\( name, count ) -> Dict.update name (updateCountBy count))
        dict1
        (Dict.toList dict2)


fromFilesToJson : List String -> Json.Encode.Value
fromFilesToJson files =
    fromFiles files
        |> toJson


fromFiles : List String -> Names
fromFiles files =
    List.map fileToNames files
        |> List.filterMap Result.toMaybe
        |> List.foldl combine empty


fileToNames : String -> Result String Names
fileToNames file =
    case Elm.Parser.parse file of
        Ok unprocessedFile ->
            Elm.Processing.process Elm.Processing.init unprocessedFile
                |> (\processedFile ->
                        fromFile processedFile
                   )
                |> Ok

        Err _ ->
            Err "Unable to parse Elm file"


fromFile : Elm.Syntax.File.File -> Names
fromFile file =
    List.map Elm.Syntax.Node.value file.declarations
        |> List.foldl accumulateNamesFromDeclaration empty


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
        , ( "ports", names.ports )
        ]


updateCount : Maybe Int -> Maybe Int
updateCount maybeInt =
    updateCountBy 1 maybeInt


updateCountBy : Int -> Maybe Int -> Maybe Int
updateCountBy amount maybeInt =
    case maybeInt of
        Just int ->
            Just (int + amount)

        Nothing ->
            Just amount


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

        Elm.Syntax.Expression.Application _ ->
            names

        Elm.Syntax.Expression.OperatorApplication _ _ _ _ ->
            names

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            names

        Elm.Syntax.Expression.IfBlock _ _ _ ->
            names

        Elm.Syntax.Expression.PrefixOperator _ ->
            names

        Elm.Syntax.Expression.Operator _ ->
            names

        Elm.Syntax.Expression.Integer _ ->
            names

        Elm.Syntax.Expression.Hex _ ->
            names

        Elm.Syntax.Expression.Floatable _ ->
            names

        Elm.Syntax.Expression.Negation _ ->
            names

        Elm.Syntax.Expression.Literal _ ->
            names

        Elm.Syntax.Expression.CharLiteral _ ->
            names

        Elm.Syntax.Expression.TupledExpression _ ->
            names

        Elm.Syntax.Expression.ParenthesizedExpression _ ->
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

        Elm.Syntax.Expression.CaseExpression _ ->
            names

        Elm.Syntax.Expression.LambdaExpression _ ->
            names

        Elm.Syntax.Expression.RecordExpr _ ->
            names

        Elm.Syntax.Expression.ListExpr _ ->
            names

        Elm.Syntax.Expression.RecordAccess _ _ ->
            names

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            names

        Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
            names

        Elm.Syntax.Expression.GLSLExpression _ ->
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
                    addNameCounts arguments names.functionArguments
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

        PortDeclaration portDeclaration ->
            let
                name =
                    Elm.Syntax.Node.value portDeclaration.name
            in
            { names
                | ports =
                    addNameCount name names.ports
            }

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

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []

        Elm.Syntax.Pattern.TuplePattern nodes ->
            List.map Elm.Syntax.Node.value nodes
                |> List.concatMap patternToNames

        Elm.Syntax.Pattern.RecordPattern _ ->
            []

        Elm.Syntax.Pattern.UnConsPattern node1 node2 ->
            List.map Elm.Syntax.Node.value [ node1, node2 ]
                |> List.concatMap patternToNames

        Elm.Syntax.Pattern.ListPattern nodes ->
            List.map Elm.Syntax.Node.value nodes
                |> List.concatMap patternToNames

        Elm.Syntax.Pattern.VarPattern string ->
            [ string ]

        Elm.Syntax.Pattern.NamedPattern _ _ ->
            []

        Elm.Syntax.Pattern.AsPattern _ nameNode ->
            Elm.Syntax.Node.value nameNode
                |> List.singleton

        Elm.Syntax.Pattern.ParenthesizedPattern _ ->
            []

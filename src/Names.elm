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
import Json.Encode


fromFile : Elm.Syntax.File.File -> Names
fromFile file =
    List.foldl accumulateNamesFromDeclaration new file.declarations


toJson : Names -> Json.Encode.Value
toJson names =
    Json.Encode.list Json.Encode.string names.functions


type alias Names =
    { functions : List String
    }


new : Names
new =
    { functions = []
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

                --FunctionImplementation =
                --    { name : Node String
                --    , arguments : List (Node Pattern)
                --    , expression : Node Expression
                --    }
            in
            { current
                | functions = name :: current.functions
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

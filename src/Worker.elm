port module Worker exposing (main)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.File
import Elm.Syntax.Node
import Json.Encode
import Platform


port outgoing : { tag : String, payload : Json.Encode.Value } -> Cmd msg


type alias Flags =
    { file : String
    }


main : Platform.Program Flags () ()
main =
    Platform.worker
        { init = \flags -> ( (), process flags )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


type alias Names =
    { functions : List String
    }


encodeNames : Names -> Json.Encode.Value
encodeNames names =
    Json.Encode.list Json.Encode.string names.functions


process : Flags -> Cmd ()
process flags =
    case fileContentToNames flags.file of
        Ok names ->
            outgoing
                { tag = "Ok"
                , payload =
                    encodeNames names
                }

        Err errors ->
            Cmd.none


fileContentToNames : String -> Result String Names
fileContentToNames moduleContent =
    case Elm.Parser.parse moduleContent of
        Ok unprocessed ->
            Elm.Processing.process Elm.Processing.init unprocessed
                |> (\processed ->
                        parsedFileToNames processed
                   )
                |> Ok

        Err errors ->
            Err "Unable to parse Elm file"


parsedFileToNames : Elm.Syntax.File.File -> Names
parsedFileToNames file =
    { functions = List.concatMap functionDeclarationNames file.declarations
    }


functionDeclarationNames :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> List String
functionDeclarationNames (Elm.Syntax.Node.Node _ outerDeclaration) =
    case outerDeclaration of
        FunctionDeclaration { declaration } ->
            let
                (Elm.Syntax.Node.Node _ functionImplementation) =
                    declaration

                (Elm.Syntax.Node.Node _ name) =
                    functionImplementation.name

                --FunctionImplementation =
                --    { name : Node String
                --    , arguments : List (Node Pattern)
                --    , expression : Node Expression
                --    }
            in
            [ name ]

        AliasDeclaration _ ->
            []

        CustomTypeDeclaration _ ->
            []

        PortDeclaration _ ->
            []

        InfixDeclaration _ ->
            []

        Destructuring _ _ ->
            []

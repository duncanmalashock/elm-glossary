port module Worker exposing (main)

import Elm.Parser
import Elm.Processing
import Json.Encode
import Names
import Platform


port outgoing : { tag : String, payload : Json.Encode.Value } -> Cmd msg


type alias Flags =
    { files : List String
    }


main : Platform.Program Flags () ()
main =
    Platform.worker
        { init = \flags -> ( (), process flags )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


process : Flags -> Cmd ()
process flags =
    outgoing
        { tag = "Ok"
        , payload = Names.toJson (filesToNames flags.files)
        }


filesToNames : List String -> Names.Names
filesToNames files =
    List.map fileToNames files
        |> List.filterMap Result.toMaybe
        |> List.foldl Names.combine Names.empty


fileToNames : String -> Result String Names.Names
fileToNames file =
    case Elm.Parser.parse file of
        Ok unprocessedFile ->
            Elm.Processing.process Elm.Processing.init unprocessedFile
                |> (\processedFile ->
                        Names.fromFile processedFile
                   )
                |> Ok

        Err parserDeadEnds ->
            Err "Unable to parse Elm file"

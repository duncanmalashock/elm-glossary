port module Worker exposing (main)

import Elm.Parser
import Elm.Processing
import Json.Encode
import Names
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


process : Flags -> Cmd ()
process flags =
    case fileToNames flags.file of
        Ok names ->
            outgoing
                { tag = "Ok"
                , payload = Names.toJson names
                }

        Err error ->
            outgoing
                { tag = "Err"
                , payload = Json.Encode.string error
                }


fileToNames : String -> Result String Names.Names
fileToNames moduleContent =
    case Elm.Parser.parse moduleContent of
        Ok unprocessedFile ->
            Elm.Processing.process Elm.Processing.init unprocessedFile
                |> (\processedFile ->
                        Names.fromFile processedFile
                   )
                |> Ok

        Err parserDeadEnds ->
            Err "Unable to parse Elm file"

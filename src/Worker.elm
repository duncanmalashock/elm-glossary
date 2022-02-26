port module Worker exposing (main)

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
        , payload = Names.fromFilesToJson flags.files
        }

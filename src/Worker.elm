port module Worker exposing (main)

import Platform


port outgoing : { tag : String, payload : Maybe String } -> Cmd msg


type alias Flags =
    { file : String
    }


main : Platform.Program Flags () ()
main =
    Platform.worker
        { init =
            \flags ->
                ( ()
                , outgoing
                    { tag = "Ok"
                    , payload = Just flags.file
                    }
                )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }

# elm-glossary
 Keep names consistent using static analysis

## Usage

Run `npm run get-names` in the terminal. This will generate a `dist/found-names.json` file.

This file contains all names used in the Elm source found in `src/Main.elm`.

## Example

Given the following `Main.elm`:
```elm
module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


init : Model
init =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    let
        increment : Int
        increment =
            1
    in
    case msg of
        Increment ->
            { model | count = model.count + increment }

        Decrement ->
            { model | count = model.count - increment }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
```

The script would produce `found-names.json`:
```json
{
  "functions": {
    "increment": 1,
    "init": 1,
    "main": 1,
    "update": 1,
    "view": 1
  },
  "functionArguments": {
    "model": 2,
    "msg": 1
  },
  "types": {
    "Msg": 1
  },
  "typeVariants": {
    "Decrement": 1,
    "Increment": 1
  },
  "typeAliases": {
    "Model": 1
  },
  "typeVariables": {},
  "ports": {}
}
```

module Escape exposing (SanitizeLevel(..), sanitize)

import Regex exposing (Regex)


type alias Replacement =
    String


type SanitizeLevel
    = Namespace
    | Element
    | State


sanitizeRegex : Regex
sanitizeRegex =
    Regex.regex "[^a-z0-9\\-_]"
        |> Regex.caseInsensitive


sanitize : SanitizeLevel -> String -> String
sanitize level str =
    case level of
        Namespace ->
            sanitize_ sanitizeRegex "" str

        Element ->
            sanitize_ sanitizeRegex "" str
                |> removeDoubleUnderscore

        State ->
            sanitize_ sanitizeRegex "" str
                |> removeDoubleUnderscore


removeDoubleUnderscore : String -> String
removeDoubleUnderscore str =
    let
        regex =
            Regex.regex "_{2,}" |> Regex.caseInsensitive
    in
        Regex.replace Regex.All regex (\_ -> "") str


sanitize_ : Regex -> Replacement -> String -> String
sanitize_ regex replacement str =
    let
        leadNumbers =
            Regex.regex "^[\\d]+"
                |> Regex.caseInsensitive
    in
        str
            |> Regex.replace Regex.All regex (\_ -> replacement)
            |> Regex.replace Regex.All leadNumbers (\_ -> "")

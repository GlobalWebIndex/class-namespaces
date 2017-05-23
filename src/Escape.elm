module Escape exposing (sanitizeNamespace, sanitize)

import Regex exposing (Regex)


type alias Replacement =
    String


sanitizeNamespace : String -> String
sanitizeNamespace =
    sanitize_


sanitize : String -> String
sanitize str =
    let
        regex =
            Regex.regex "_{2,}|-{2,}" |> Regex.caseInsensitive
    in
        sanitize_ str
            |> Regex.replace Regex.All regex (\_ -> "")


sanitize_ : String -> String
sanitize_ str =
    let
        sanitizeRegex =
            Regex.regex "[^a-z0-9\\-_]"
                |> Regex.caseInsensitive

        leadNumbers =
            Regex.regex "^[\\d]+"
                |> Regex.caseInsensitive

        replacement =
            (\_ -> "")
    in
        str
            |> Regex.replace Regex.All sanitizeRegex replacement
            |> Regex.replace Regex.All leadNumbers replacement

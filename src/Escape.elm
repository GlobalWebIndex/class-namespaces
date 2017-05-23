module Escape exposing (sanitizeNamespace, sanitize)

import Regex exposing (Regex)


sanitizeNamespace : String -> String
sanitizeNamespace str =
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


sanitize : String -> String
sanitize str =
    let
        regex =
            Regex.regex "_{2,}|-{2,}" |> Regex.caseInsensitive
    in
        sanitizeNamespace str
            |> Regex.replace Regex.All regex (\_ -> "")

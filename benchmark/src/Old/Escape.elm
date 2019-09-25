module Old.Escape exposing (sanitize, sanitizeNamespace)

import Regex exposing (Regex)


caseInsensitiveOption : Regex.Options
caseInsensitiveOption =
    { caseInsensitive = True
    , multiline = False
    }


removeByRegex regex string =
    Regex.fromStringWith caseInsensitiveOption regex
        |> Maybe.map (\r -> String.toLower <| Regex.replace r (\_ -> "") string)
        |> Maybe.withDefault string


sanitizeNamespace : String -> String
sanitizeNamespace str =
    str
        |> removeByRegex "[^a-z0-9\\-_]"
        |> removeByRegex "^[\\d]+"


sanitize : String -> String
sanitize str =
    removeByRegex "_{2,}|-{2,}" <| sanitizeNamespace str

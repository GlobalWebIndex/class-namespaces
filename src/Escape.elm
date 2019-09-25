module Escape exposing
    ( sanitize
    , sanitizeNamespace
    )

import Regex exposing (Regex)


sanitizeNamespace : String -> String
sanitizeNamespace =
    removeByRegex stuffInvalidInNamespace


sanitize : String -> String
sanitize =
    removeByRegex stuffInvalidInClass


removeByRegex : Regex -> String -> String
removeByRegex regex =
    String.toLower << Regex.replace regex (\_ -> "")


stuffInvalidInNamespace : Regex
stuffInvalidInNamespace =
    Maybe.withDefault Regex.never <| Regex.fromString "^[^_a-zA-Z]+|[^\\w\\-]"


{-| This regex will match:

  - all chars at the beginning which are not letters or '\_'
  - any chars which are not in: a-zA-Z0-9\_ (corresponds to \\w) and '-'
  - 2 or more consecutive occurrences of '\_' '-'

Based on <https://stackoverflow.com/questions/448981/which-characters-are-valid-in-css-class-names-selectors#answer-449000>

-}
stuffInvalidInClass : Regex
stuffInvalidInClass =
    Maybe.withDefault Regex.never <| Regex.fromString "^[^_a-zA-Z]+|[^\\w\\-]|_{2,}|-{2,}"

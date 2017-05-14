module NamespaceClass
    exposing
        ( ClassName
        , namespace
        , element
        , toClass
        , nested
        , withStates
        )

import Html exposing (Attribute)
import Html.Attributes exposing (class)


type ClassName
    = ClassName String (List String)


namespace : String -> ClassName
namespace name =
    ClassName name []


element : String -> ClassName -> ClassName
element name (ClassName namespace list) =
    ClassName namespace <| name :: list


toString : ClassName -> String
toString (ClassName namespace list) =
    let
        spacer acc =
            if String.isEmpty acc then
                ""
            else
                "--"

        addElement name acc =
            acc ++ (spacer acc) ++ name
    in
        case list of
            [] ->
                namespace

            head :: tail ->
                namespace
                    ++ "__"
                    ++ List.foldr addElement "" list


toStringWithStates : (List String) -> ClassName -> String
toStringWithStates states className =
    toString className ++ List.foldl (\s acc -> acc ++ " " ++ s) "" states


toClass : ClassName -> Attribute msg
toClass =
    class << toString


nested : String -> ClassName -> Attribute msg
nested name =
    toClass << element name


withStates : (List String) -> ClassName -> Attribute msg
withStates state =
    class << (toStringWithStates state)

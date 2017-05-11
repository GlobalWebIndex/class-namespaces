module NamespaceClass exposing (ClassName, block, element, nclass, wclass)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


type ClassName
    = ClassName String (List String)


block : String -> ClassName
block name =
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


nclass : ClassName -> Attribute msg
nclass =
    class << toString


wclass : String -> ClassName -> Attribute msg
wclass name =
    nclass << element name

module NamespaceClass
    exposing
        ( ClassName
        , namespace
        , element
        , toClass
        , withClass
        , withState
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


toStringWithState : String -> ClassName -> String
toStringWithState state_ className =
    let
        state =
            String.trim state_
    in
        case state of
            "" ->
                toString className

            _ ->
                toString className ++ " " ++ state


toClass : ClassName -> Attribute msg
toClass =
    class << toString


withClass : String -> ClassName -> Attribute msg
withClass name =
    toClass << element name


withState : String -> ClassName -> Attribute msg
withState state =
    class << (toStringWithState state)

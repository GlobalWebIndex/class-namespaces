module NamespaceClass
    exposing
        ( ClassName
        , namespace
        , element
        , toClass
        , nested
        , withStates
        )

{-| Abstraction for generating convetional class names

# Type and Constructor

@docs ClassName, namespace

# Adding Elements

@docs element

# Convert to Attribute

@docs toClass, element, nested, withStates

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import Escape


-- Type


{-| ClassName type describes selector used to style element.
-}
type ClassName
    = ClassName String (List String)


{-| Construct [`ClassName`](#ClassName) with given namespace

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu" |> toClass
    class "menu"
-}
namespace : String -> ClassName
namespace name =
    ClassName (Escape.sanitize Escape.Namespace name) []



-- Add nested element


{-| Add element

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> element "list"
    ... |> toClass
    class "menu__list"
-}
element : String -> ClassName -> ClassName
element name (ClassName namespace list) =
    ClassName namespace <| (Escape.sanitize Escape.Element name) :: list



-- Convert to Html.Attribute


{-| Convert [`ClassName`](#ClassName) to `Html.Attribute msg`

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> toClass
    class "menu"

    >>> namespace "menu"
    ... |> element "list"
    ... |> toClass
    class "menu__list"
-}
toClass : ClassName -> Attribute msg
toClass =
    class << toString


{-| Add new element and turn to `Html.Attribute msg`

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> nested "item"
    class "menu__item"

    >>> namespace "menu"
    ... |> element "item"
    ... |> nested "link"
    class "menu__item--link"
-}
nested : String -> ClassName -> Attribute msg
nested name =
    toClass << element name


{-| Add state to [`ClassName`](#ClassName) and turn to `Html.Attrinute msg`

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> element "item"
    ... |> withStates ["active", "highlighted"]
    class "menu__item active highlighted"

    >>> namespace "menu"
    ... |> withStates []
    class "menu"
-}
withStates : List String -> ClassName -> Attribute msg
withStates state =
    class << (toStringWithStates state)



-- Private


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


toStringWithStates : List String -> ClassName -> String
toStringWithStates states className =
    toString className ++ List.foldl (\s acc -> acc ++ " " ++ (Escape.sanitize Escape.State s)) "" states

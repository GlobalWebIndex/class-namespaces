module WeakCss
    exposing
        ( ClassName
        , add
        , addMany
        , namespace
        , nest
        , nestMany
        , toClass
        , withStates
        )

{-| Abstraction for working with [`Weak Css`](https://github.com/GlobalWebIndex/weak-css)
style class names.


# Type and Constructor

@docs ClassName, namespace


# Adding Elements

@docs add, addMany


# Convert to Attribute

@docs toClass, nest, nestMany, withStates

-}

import Escape
import Html exposing (Attribute)
import Html.Attributes exposing (class)


-- Type


{-| ClassName type describes class selector used to style element.

All strings are sanitized to prevent missuse and odd resulting selectors.
**It's highly recommended to avoid spaces, `__` and `--` in arguments though.**

-}
type ClassName
    = ClassName String (List String)


{-| Construct [`ClassName`](#ClassName) with given namespace.

    import Html.Attributes exposing (class)

    namespace "menu" |> toClass

    --> class "menu"

-}
namespace : String -> ClassName
namespace name =
    ClassName (Escape.sanitizeNamespace name) []



-- Add nested element to element path.


{-| Add element

    import Html.Attributes exposing (class)

    namespace "menu"
        |> add "list"
        |> toClass

    --> class "menu__list"

-}
add : String -> ClassName -> ClassName
add name (ClassName classNamespace list) =
    ClassName classNamespace <| Escape.sanitize name :: list


{-| Add new elements from list.

    import Html.Attributes exposing (class)

    namespace "menu"
        |> addMany ["item", "link"]
        |> toClass

    --> class "menu__item--link"

-}
addMany : List String -> ClassName -> ClassName
addMany listToAdd className =
    List.map Escape.sanitize listToAdd
        |> List.foldl add className



-- Convert to Html.Attribute


{-| Convert [`ClassName`](#ClassName) to `Html.Attribute msg`.

    import Html.Attributes exposing (class)

    namespace "menu"
        |> toClass
    --> class "menu"

    namespace "menu"
        |> add "list"
        |> toClass
    --> class "menu__list"

-}
toClass : ClassName -> Attribute msg
toClass =
    class << toString


{-| Add new element and convert to `Html.Attribute msg`.

    import Html.Attributes exposing (class)

    namespace "menu"
        |> nest "item"
    --> class "menu__item"

    namespace "menu"
        |> add "item"
        |> nest "link"
    --> class "menu__item--link"

-}
nest : String -> ClassName -> Attribute msg
nest name =
    toClass << add name


{-| Add new elements from list and convert it all to `Html.Attribute msg`.

    import Html.Attributes exposing (class)

    namespace "menu"
        |> nestMany ["item", "link"]
    --> class "menu__item--link"

    namespace "menu"
        |> nestMany ["item"]
    --> class "menu__item"

-}
nestMany : List String -> ClassName -> Attribute msg
nestMany listToAdd =
    toClass << addMany listToAdd


{-| Add state to last element [`ClassName`](#ClassName) and convert to `Html.Attribute msg`.

    import Html.Attributes exposing (class)

    isActive : Bool
    isActive =
        True

    isHighlighted : Bool
    isHighlighted =
        False

    namespace "menu"
        |> add "item"
        |> withStates
            [ ( "active", isActive )
            , ( "highlighted", isHighlighted )
            ]
    --> class "menu__item active"

    namespace "menu"
        |> withStates []
    --> class "menu"

-}
withStates : List ( String, Bool ) -> ClassName -> Attribute msg
withStates states =
    class << toStringWithStates states



-- Private


toString : ClassName -> String
toString (ClassName classNamespace list) =
    let
        spacer acc =
            if String.isEmpty acc then
                ""
            else
                "--"

        foldElement name acc =
            acc ++ spacer acc ++ name
    in
    case list of
        [] ->
            classNamespace

        head :: tail ->
            classNamespace
                ++ "__"
                ++ List.foldr foldElement "" list


toStringWithStates : List ( String, Bool ) -> ClassName -> String
toStringWithStates states className =
    let
        activeStates : List String
        activeStates =
            states
                |> List.filterMap
                    (\( string, state ) ->
                        if state then
                            Just string
                        else
                            Nothing
                    )
    in
    toString className ++ List.foldl (\s acc -> acc ++ " " ++ Escape.sanitize s) "" activeStates

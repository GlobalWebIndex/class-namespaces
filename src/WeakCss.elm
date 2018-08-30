module WeakCss exposing
    ( ClassName, namespace
    , addElement
    , toClass, nested, withStates
    , addMany, nestMany
    )

{-| Abstraction for working with [`Weak Css`](https://github.com/GlobalWebIndex/weak-css)
style class names.


# Type and Constructor

@docs ClassName, namespace


# Adding Elements

@docs addElement


# Convert to Attribute

@docs toClass, nested, withStates

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

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu" |> toClass
    class "menu"

-}
namespace : String -> ClassName
namespace name =
    ClassName (Escape.sanitizeNamespace name) []



-- Add nested element to element path.


{-| Add element

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> addElement "list"
    ... |> toClass
    class "menu__list"

-}
addElement : String -> ClassName -> ClassName
addElement name (ClassName classNamespace list) =
    ClassName classNamespace <| Escape.sanitize name :: list


{-| Add new elements from list.

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> addMany ["item", "link"]
    ... |> toClass
    class "menu__item--link"

-}
addMany : List String -> ClassName -> ClassName
addMany listToAdd className =
    List.map Escape.sanitize listToAdd
        |> List.foldl addElement className



-- Convert to Html.Attribute


{-| Convert [`ClassName`](#ClassName) to `Html.Attribute msg`.

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> toClass
    class "menu"

    >>> namespace "menu"
    ... |> addElement "list"
    ... |> toClass
    class "menu__list"

-}
toClass : ClassName -> Attribute msg
toClass =
    class << toString


{-| Add new element and convert to `Html.Attribute msg`.

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> nested "item"
    class "menu__item"

    >>> namespace "menu"
    ... |> addElement "item"
    ... |> nested "link"
    class "menu__item--link"

-}
nested : String -> ClassName -> Attribute msg
nested name =
    toClass << addElement name


{-| Add new elements from list and convert it all to `Html.Attribute msg`.

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> nestMany ["item", "link"]
    class "menu__item--link"

    >>> namespace "menu"
    ... |> nestMany ["item"]
    class "menu__item"

-}
nestMany : List String -> ClassName -> Attribute msg
nestMany listToAdd =
    toClass << addMany listToAdd


{-| Add state to last element [`ClassName`](#ClassName) and convert to `Html.Attrinute msg`.

    >>> import Html.Attributes exposing (class)

    >>> namespace "menu"
    ... |> addElement "item"
    ... |> withStates ["active", "highlighted"]
    class "menu__item active highlighted"

    >>> namespace "menu"
    ... |> withStates []
    class "menu"

-}
withStates : List String -> ClassName -> Attribute msg
withStates state =
    class << toStringWithStates state



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


toStringWithStates : List String -> ClassName -> String
toStringWithStates states className =
    toString className ++ List.foldl (\s acc -> acc ++ " " ++ Escape.sanitize s) "" states

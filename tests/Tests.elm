module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Html.Attributes exposing (class)


-- lib

import NamespaceClass exposing (..)


b =
    namespace "menu"


e =
    b |> element "list"


all : Test
all =
    describe "Namespace Class"
        [ test "block" <|
            \() ->
                toClass b
                    |> Expect.equal (class "menu")
        , test "element" <|
            \() ->
                toClass e
                    |> Expect.equal (class "menu__list")
        , test "wclass" <|
            \() ->
                withClass "item" e
                    |> Expect.equal (class "menu__list--item")
        , test "more elements" <|
            \() ->
                e
                    |> element "item"
                    |> withClass "link"
                    |> Expect.equal (class "menu__list--item--link")
        , test "with state" <|
            \() ->
                e
                    |> element "item"
                    |> withState "active"
                    |> Expect.equal (class "menu__list--item active")
        ]

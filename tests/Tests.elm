module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Html.Attributes exposing (class)


-- lib

import NamespaceClass exposing (..)


namespaced =
    namespace "menu"


inElement =
    element "list" namespaced


all : Test
all =
    describe "Namespace Class"
        [ test "block" <|
            \() ->
                toClass namespaced
                    |> Expect.equal (class "menu")
        , test "element" <|
            \() ->
                toClass inElement
                    |> Expect.equal (class "menu__list")
        , test "wclass" <|
            \() ->
                nested "item" inElement
                    |> Expect.equal (class "menu__list--item")
        , test "more elements" <|
            \() ->
                inElement
                    |> element "item"
                    |> nested "link"
                    |> Expect.equal (class "menu__list--item--link")
        , test "with state" <|
            \() ->
                inElement
                    |> element "item"
                    |> withState "active"
                    |> Expect.equal (class "menu__list--item active")
        ]

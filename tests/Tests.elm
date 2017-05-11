module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Html.Attributes exposing (class)


-- lib

import NamespaceClass exposing (..)


b =
    block "menu"


e =
    b |> element "list"


all : Test
all =
    describe "Namespace Class"
        [ test "block" <|
            \() ->
                nclass b
                    |> Expect.equal (class "menu")
        , test "element" <|
            \() ->
                nclass e
                    |> Expect.equal (class "menu__list")
        , test "wclass" <|
            \() ->
                wclass "item" e
                    |> Expect.equal (class "menu__list--item")
        , test "more elements" <|
            \() ->
                e
                    |> element "item"
                    |> wclass "link"
                    |> Expect.equal (class "menu__list--item--link")
        ]

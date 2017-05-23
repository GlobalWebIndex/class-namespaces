module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Html.Attributes exposing (class)


-- lib

import NamespaceClass exposing (..)
import Escape


namespaced =
    namespace "menu"


inElement =
    element "list" namespaced


all : Test
all =
    describe "All tests"
        [ namespaceClassTest
        , sanitizeTest
        ]


namespaceClassTest : Test
namespaceClassTest =
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
                    |> withStates [ "active", "edited" ]
                    |> Expect.equal (class "menu__list--item active edited")
        , test "with empty state" <|
            \() ->
                inElement
                    |> element "item"
                    |> withStates []
                    |> Expect.equal (class "menu__list--item")
        ]


sanitizeTest : Test
sanitizeTest =
    describe "Sanitize string"
        [ test "namespace - remove invalid chars" <|
            \() ->
                Escape.sanitize Escape.Namespace "invalid string ěščěšč"
                    |> Expect.equal "invalidstring"
        , test "namespace - remove invalid and keep '__'" <|
            \() ->
                Escape.sanitize Escape.Namespace " invalid string__module "
                    |> Expect.equal "invalidstring__module"
        , test "namespace - class name valid against standards" <|
            \() ->
                Escape.sanitize Escape.Namespace " 98invalid st-ring__module"
                    |> Expect.equal "invalidst-ring__module"
        , test "element - remove all invalid, keep '-' and '_'" <|
            \() ->
                Escape.sanitize Escape.Element " 98invalid st-ring__name"
                    |> Expect.equal "invalidst-ringname"
        , test "state - remove all invalid, keep '-' and '_'" <|
            \() ->
                Escape.sanitize Escape.State " inva_lid st-ring__name "
                    |> Expect.equal "inva_lidst-ringname"
        ]

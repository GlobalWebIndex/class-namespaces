module Tests exposing (escapeTest, inElement, namespaceClassTest, namespaced)

-- lib

import Escape
import Expect
import Html.Attributes exposing (class)
import Test exposing (..)
import WeakCss exposing (..)


namespaced =
    namespace "menu"


inElement =
    addElement "list" namespaced


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
        , test "add multiple elements at once (addMany)" <|
            \() ->
                namespaced
                    |> addMany [ "list", "item", "link" ]
                    |> toClass
                    |> Expect.equal (class "menu__list--item--link")
        , test "more elements" <|
            \() ->
                inElement
                    |> addElement "item"
                    |> nested "link"
                    |> Expect.equal (class "menu__list--item--link")
        , test "multiple nested elements added at once (nestMany)" <|
            \() ->
                namespaced
                    |> nestMany [ "list", "item", "link" ]
                    |> Expect.equal (class "menu__list--item--link")
        , test "with state" <|
            \() ->
                inElement
                    |> addElement "item"
                    |> withStates [ "active", "edited" ]
                    |> Expect.equal (class "menu__list--item active edited")
        , test "with empty state" <|
            \() ->
                inElement
                    |> addElement "item"
                    |> withStates []
                    |> Expect.equal (class "menu__list--item")
        ]


escapeTest : Test
escapeTest =
    describe "Escape"
        [ test "namespace - remove invalid chars" <|
            \() ->
                Escape.sanitizeNamespace "invalid string ěščěšč"
                    |> Expect.equal "invalidstring"
        , test "namespace - remove invalid and keep '__'" <|
            \() ->
                Escape.sanitizeNamespace " invalid string__module "
                    |> Expect.equal "invalidstring__module"
        , test "namespace - class name valid against standards" <|
            \() ->
                Escape.sanitizeNamespace " 98invalid st-ring__module"
                    |> Expect.equal "invalidst-ring__module"
        , test "sanitize - remove all invalid, keep '-' and '_'" <|
            \() ->
                Escape.sanitize " inva_lid st-ring__name "
                    |> Expect.equal "inva_lidst-ringname"
        , test "sanitize - remove all invalid, multi '_' or '-', keep '-' and '_'" <|
            \() ->
                Escape.sanitize " inva_lid st-ring__name--some-end "
                    |> Expect.equal "inva_lidst-ringnamesome-end"
        ]

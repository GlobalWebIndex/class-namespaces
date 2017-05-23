# Class Namespaces

[![Build Status](https://travis-ci.org/GlobalWebIndex/class-namespaces.svg?branch=master)](https://travis-ci.org/GlobalWebIndex/class-namespaces)

Abstraction for easier work with conventional namespaced class names in Elm + TEA.
Currently only our in-house [WeakCss](https://github.com/GlobalWebIndex/weak-css) style is support.
We're considering adding classic [BEM](http://getbem.com/) style support in future though.

## Installation

Usual elm package install:

```
$ elm package install GlobalWebIndex/class-namespaces
```

## Usage

Just simply use `WeakCss` module's API instead of directly usin `Html.Attributes.class`.

```elm
import Html exposing (Html, Attribute)
import WeakCss exposing (ClassName)

moduleClass : ClassName
moduleClass =
    WeakCss.namespace "menu"

view : Html msg
view =
    let
        navClass =
            moduleClass
                |> WeakCss.element "nav"

        itemClass =
            navClass
                |> WeakCss.element "item"

    in
        Html.aside
            [ moduleClass |> WeakCss.toClass ]
            [ Html.nav
                [ navClass |> WeakCss.toClass ]
                [ Html.ul
                    [ navClass |> nested "list" ]
                    [ Html.li [ itemClass |> WeakCss.withState [] ] [ Html.text "first item"]
                    , Html.li [ itemClass |> WeakCss.withState ["active"] ] [ Html.text "second active item"]
                    ]
                ]
            ]
```

This is structure of corresponding CSS file written in [SCSS](http://sass-lang.com/):

```scss
.menu {
    &__nav {
        &--list {
        }

        &--item {
            &.active {
            }
        }
    }
}
```

And this is how selectors in compiled CSS file will look like:

```css
.menu {}
.menu__nav {}
.menu__nav--list {}
.menu__nav--item {}
.menu__nav--item.active {}
```

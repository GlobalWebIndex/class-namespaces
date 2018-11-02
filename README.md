# Class Namespaces

[![Build Status](https://travis-ci.org/GlobalWebIndex/class-namespaces.svg?branch=master)](https://travis-ci.org/GlobalWebIndex/class-namespaces)

Abstraction for easier work with conventional namespaced class names in Elm + TEA.
Currently only our in-house [WeakCss](https://github.com/GlobalWebIndex/weak-css) style is supported.
We're considering adding classic [BEM](http://getbem.com/) style support in future though.

**This package is highly experimental and might change a lot over time.**

Feedback and contributions to both code and documentation are very welcome.

## Installation

Usual elm package install:

```
$ elm package install GlobalWebIndex/class-namespaces
```

## Usage

Just simply use `WeakCss` module's API instead of using `Html.Attributes.class` directly.

```elm
module Main exposing (..)

import Html exposing (Attribute, Html)
import WeakCss exposing (ClassName)


moduleClass : ClassName
moduleClass =
    WeakCss.namespace "menu"


main : Html msg
main =
    let
        navClass =
            moduleClass
                |> WeakCss.add "nav"

        itemClass =
            navClass
                |> WeakCss.add "item"

        isActive =
            -- presumably some actual logic here
            True
    in
    Html.aside
        [ moduleClass |> WeakCss.toClass ]
        [ Html.nav
            [ navClass |> WeakCss.toClass ]
            [ Html.ul
                [ navClass |> WeakCss.nest "list" ]
                [ Html.li [ itemClass |> WeakCss.withStates [] ] [ Html.text "first item" ]
                , Html.li [ itemClass |> WeakCss.withStates [ (isActive, "active") ] ] [ Html.text "second active item" ]
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

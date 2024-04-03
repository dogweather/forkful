---
changelog:
- 2024-02-27, dogweather, edited and tested
date: 2024-02-27 22:04:38.367952-07:00
description: "Generating random numbers in Elm involves using the `Random` module\
  \ to produce pseudo-random numbers, which come in handy for a variety of tasks such\
  \ as\u2026"
lastmod: '2024-03-13T22:45:00.005764-06:00'
model: gpt-4-0125-preview
summary: Generating random numbers in Elm involves using the `Random` module to produce
  pseudo-random numbers, which come in handy for a variety of tasks such as games,
  simulations, and even as part of algorithms that require stochastic processes.
title: Generating random numbers
weight: 12
---

## How to:
Elm's pure functional nature means that you can't generate random numbers directly as you might in imperative languages. Instead, you use the `Random` module in conjunction with commands. Here's a basic example that generates a random integer between 1 and 100.

First, install the `Random` module with `elm install elm/random`. Then import it into your Elm file, along with the necessary HTML and event modules, like so:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

For this to be a self-contained example, you can add this boilerplate:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Next, define a **command** to generate a random number. This involves setting up a `Msg` type to handle the random number once it's generated, a `Model` to store it, and an update function to tie it all together.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

To trigger a number generation, you would send a `Generate` message, for instance, through a button in your view:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Random Number: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generate" ]
        ]
```

When you click the "Generate" button, a random number between 1 and 100 will be displayed.

This simplistic approach can be adapted and expanded, leveraging other functions in the `Random` module to produce random floats, lists, or even complex data structures based on custom types, providing a vast playground for adding unpredictability to your Elm applications.

The Elm Guide goes into much more detail. It also has [an example of rolling a six-sided die](https://guide.elm-lang.org/effects/random).

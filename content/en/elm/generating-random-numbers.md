---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random numbers are unpredictable values produced by a generation process. Programmers use these for simulating unpredictable patterns, data encryption, or for games and simulations.

## How to:

Elm provides the `Random` module for generating random numbers. Here's a simple example of generating a random integer in Elm:

```Elm
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random

type alias Model = Int

main =
  Html.beginnerProgram { model = 0, view = view, update = update }

type Msg = Generate

update msg model =
  case msg of
    Generate ->
      ( model, Random.generate NewNumber (Random.int 1 100) )

type Msg = Generate | NewNumber Int

update msg model =
  case msg of
    Generate ->
      ( model, Random.generate NewNumber (Random.int 1 100) )

    NewNumber newNum ->
      ( newNum, Cmd.none )

view model =
  div []
    [ button [ onClick Generate ] [ text "Generate Random Number" ]
    , div [] [ text (toString model) ]
    ]
```
In the above code, when you click on the "Generate Random Number" button, Elm generates a random integer between 1 and 100.

## Deep Dive

Historically, the randomness of numbers has proven important in cryptography, simulations, and probability-based algorithms. Elm's random number generation architecture uses Elm's command (Cmd) to create side effects, assuring that functions remain pure.

Elm's `Random` module provides a useful set of functions, including generating integers, floats, and even lists of random values. It's different from some other language standard libraries, which implement random functions impurely. If you need more complex distributions or random generators, you might explore third-party packages.

Elm's purity can make random number generation feel complex. It's crucial to understand that "Random.generate" is asynchronous; it lets you request a random value, but you don't directly get the number; it comes as a message via "NewNumber" function.

## See Also

- Elm's official Random module documentation: [Random (package.elm-lang.org)](https://package.elm-lang.org/packages/elm/core/latest/Random)
- An in-depth guide on Elm's effects (Cmd): [Elm - Understanding Effects (youtu.be)](https://youtu.be/6EdXaWfoslc)
- For more information on the basics of Elm: [Introduction to Elm (elm-lang.org)](https://guide.elm-lang.org/)
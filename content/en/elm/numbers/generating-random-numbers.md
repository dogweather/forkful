---
title:                "Generating random numbers"
date:                  2024-01-27T20:26:19.358617-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers in Elm involves creating unpredictable numerical values that are essential for applications like games, simulations, and security algorithms. Programmers use randomness to simulate real-world variability, enhance user experience, or secure data with encryption techniques.

## How to:
Elm handles randomness differently than many programming languages, utilizing a system that keeps functions pure. To generate random numbers, you must work with Elm's `Random` module. Here's a basic example of generating a random number between 1 and 100:

```Elm
import Html exposing (Html, text)
import Random

main : Html msg
main =
    Random.generate NewRandomNumber (Random.int 1 100)
    |> Html.map (text << toString)

type Msg = NewRandomNumber Int
```

This snippet uses `Random.generate` to create a command that, when executed, produces a random number within the specified range. The `type Msg` declaration is used to handle the generated number in your Elm application's update function.

For a more interactive example, let's look at a scenario where users trigger random number generation through a click:

```Elm
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random

type alias Model = Int

type Msg = Generate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            (model, Random.generate NewRandomNumber (Random.int 1 100))

view : Model -> Html Msg
view model =
    div []
        [ text ("Generated number: " ++ String.fromInt model)
        , button [ onClick Generate ] [ text "Generate new number" ]
        ]

type Msg = NewRandomNumber Int
```

This Elm application introduces interactivity, updating the display with a new random number each time the user clicks the button.

## Deep Dive
The design of Elm's random number generation system stems from the language's commitment to purity and predictability. Instead of direct, impure functions that return different values on each call, Elm encapsulates randomness in a `Cmd` structure, aligning with its architecture that separates side effects from pure functions.

While this approach guarantees consistency in application behavior and facilitates debugging, it introduces a learning curve for those accustomed to the imperative generation of random numbers. However, the benefits of maintaining application purity and the ease of testing often outweigh the initial complexity.

Elm's method also contrasts with languages that offer global random number generators, which can lead to subtle bugs due to shared state. By requiring explicit handling of random number generation and its effects, Elm encourages developers to think more critically about where and how randomness affects their applications, leading to more robust and predictable code.

For alternatives, other functional languages offer similar functionalities but may implement them differently. Haskell, for example, also maintains purity in random number generation but through the use of monads, a concept that Elm deliberately avoids to simplify its model. Comparatively, Elm's approach is more accessible to newcomers and emphasizes a straightforward application architecture without sacrificing the power of functional programming principles.

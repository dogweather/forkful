---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:53.629264-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers means creating unpredictable numerical values. Programmers use randomness for games, simulations, tests, and whenever they need an element of surprise or diversity in their code.

## How to:

Elm handles randomness through 'Random' module functions. Here's how you produce a random integer between 1 and 100:

```Elm
import Random

randomInt : Cmd Msg
randomInt =
    Random.generate NewRandomNumber (Random.int 1 100)

type Msg = NewRandomNumber Int

-- In your update function handle the NewRandomNumber Msg
-- update msg model =
--     case msg of
--         NewRandomNumber number ->
--             ( { model | randomNumber = number }, Cmd.none )
```

Add the appropriate message and update function logic to make use of the new random number as needed.

## Deep Dive

Historically, randomness in computer programs has always been pseudo-random; that's calculations based on algorithms, not truly random. Elm's approach to randomness is purely functional, ensuring reproducibility and testability. Alternative methods, like using JavaScript interop, exist but are not idiomatic Elm. The 'Random' module uses a seed-based generator which is predictable if you know the initial seed. This can be useful for debugging or games that need a replay feature with the same sequence of events.

## See Also:

To dive into Elm's randomness, check these out:

- Elm's official documentation on randomness:
  [Elm - Random](https://package.elm-lang.org/packages/elm/random/latest/)
- A broader explanation of pseudo-randomness:
  [Wikipedia - Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- To understand Elm's approach to effects like randomness:
  [Elm - Commands and Subscriptions](https://guide.elm-lang.org/effects/)
---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:04.716214-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Elm is about creating unpredictable values that can be used for various purposes such as simulations, games, or any application that requires an element of unpredictability. Programmers do it to introduce variability and make their applications more dynamic and engaging.

## How to:

Elm handles randomness differently than many programming languages. Instead of directly returning a random number, Elm uses a system of commands and subscriptions to ensure that your program remains pure and predictable. Here's a basic guide on generating random numbers.

First, add the `Random` module import to your Elm file:

```Elm
import Random
```

Then, define a command to generate a random number. Let's create a random integer between 1 and 100:

```Elm
randomIntCmd : Cmd Msg
randomIntCmd =
    Random.generate NewRandomInt (Random.int 1 100)
```

Here, `Random.generate` creates a command that, when executed, sends a message of type `Msg` with the random integer. `NewRandomInt` is a custom message type you must define in your `Msg` type. For example:

```Elm
type Msg
    = NewRandomInt Int
```

In your `update` function, handle the `NewRandomInt` message to process the random integer:

```Elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewRandomInt n ->
            ({ model | randomNumber = n }, Cmd.none)
```

This merely updates the model with the new random number. Remember to define `randomNumber` in your model.

Finally, to trigger the generation, you can call `randomIntCmd` as a command in response to some user action or lifecycle event.

Sample output cannot be directly shown because each execution generates a random number.

## Deep Dive

Elm's approach to randomness, rooted in its architecture, respects the language's emphasis on purity and predictability. Unlike imperative languages, where a function like `rand()` might return a new random number on each call, Elm encapsulates effects such as random number generation within its architecture of commands and subscriptions. This ensures that Elm programs remain easy to test and debug, as their behavior is more predictable and less dependent on external state.

This model is inspired by the Elm Architecture and borrows concepts from functional reactive programming (FRP). While it may seem cumbersome compared to direct method calls in languages like JavaScript or Python, this design guarantees that Elm applications maintain a predictable flow of data, making them easier to maintain and evolve.

Critics might argue that this architecture introduces boilerplate and complexity for simple tasks like generating a random number. However, the benefits of maintainability, testability, and predictability generally outweigh these concerns. For use cases requiring extensive random number generation or more direct control over randomness, external libraries or ports to JavaScript could be considered as alternatives, although they may compromise some of the guarantees provided by Elm's architecture.

## See also

### Official Elm Documentation
- [elm/random Package](https://package.elm-lang.org/packages/elm/random/latest/)

### Tutorials and Guides
- **Medium Articles**: [Introduction to Random Generators in Elm](https://medium.com/@_rchaves_/introduction-to-random-generators-in-elm-2b112b7fcd34)
- **Practical Examples**: [Elm Random Example on Ellie](https://ellie-app.com/new) *(Note: Search for "Random" examples after navigating to the link)*

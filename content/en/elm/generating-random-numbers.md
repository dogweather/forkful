---
title:                "Elm recipe: Generating random numbers"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why 
Generating random numbers is a common task in computer programming, often used for games, simulations, and data analysis. By using randomly generated numbers, we can add an element of unpredictability and variation to our programs. This can make them more realistic, interesting, and even more secure.

## How To
To generate random numbers in Elm, we can use the `Random` module. First, we need to import it:
```elm
import Random
```
Next, we can use the `generate` function to create a random number within a specific range. For example, let's generate a random number between 1 and 10:
```elm
Random.generate (\_ -> Random.int 1 10)
```
This will return a `Cmd Int` object, which can be used in our `update` function to modify our model. We can also use `map` to convert the `Cmd Int` to a `Msg` that can be handled by our `update` function:
```elm
Random.generate (\_ -> Random.int 1 10)
    |> Cmd.map SetRandomNumber
```
In this example, we have a `SetRandomNumber` message that takes an `Int` parameter, which we can use in our `update` function to set the random number in our model.

## Deep Dive
Behind the scenes, the `Random` module uses a pseudo-random number generator (PRNG) to generate our random numbers. This means that the numbers are not truly random, but rather follow a deterministic algorithm based on a seed value. By providing a seed value, we can control and reproduce the sequence of generated numbers.

If we want to generate more complex data structures, such as lists or custom types, we can use `Random.Generator` to create a generator for our desired structure and then use `Random.generate` to produce a value from the generator.

## See Also
- [Elm documentation on Random](https://package.elm-lang.org/packages/elm/random/latest/)
- [Article on the importance of random numbers in programming](https://medium.com/@bmbouter/random-numbers-in-computer-programs-are-important-ebcb3a28d8b9)
- [Tutorial on generating random numbers in Elm with examples](https://www.elm-tutorial.org/en/01-foundations/online-06-random.html)
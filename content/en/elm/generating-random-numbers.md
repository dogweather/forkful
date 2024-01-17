---
title:                "Generating random numbers"
html_title:           "Elm recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in programming refers to the ability to generate a sequence of numbers that appear to be random. Programmers use random numbers for a variety of reasons, such as creating randomized games or simulations, generating unique identifiers, or implementing algorithms that require some element of randomness.

## How to:

The `Random` module in Elm allows us to easily generate random numbers and use them in our programs. Here's how to do it:

```
import Random exposing (..)

-- Generate a random integer between 1 and 10
randomInt : Int
randomInt =
  Random.int 1 10
```

The `Random.int` function takes two arguments: the minimum and maximum values for the range of numbers we want to generate. In the example above, the generated number will be between 1 and 10. We can also generate random floating-point numbers using `Random.float`.

```
-- Generate a random floating-point number between 0 and 1
randomFloat : Float
randomFloat =
  Random.float 0 1
```

We can also use random numbers to make decisions in our code. For example, we can use `Random.generate` to generate a random boolean value and use it in a conditional statement.

```
-- Generate a random boolean value and use it in a conditional statement
randomBool : Bool
randomBool =
  Random.generate (always False)
    (Random.bool 0.5)
```

In this example, there is a 50% chance of `randomBool` being `True` and a 50% chance of it being `False`.

## Deep Dive:

Generating random numbers has been a part of programming since its early days. In the past, programmers often relied on using mathematical formulas to generate pseudo-random numbers. However, these methods were not truly random and led to patterns in the generated numbers.

In contrast, the `Random` module in Elm uses a cryptographically secure random number generator to ensure truly random output. This generator uses a variety of sources, such as the internal clock, user input, and memory, to produce random numbers.

While the `Random` module is the recommended way to generate random numbers in Elm, there are other alternatives such as using JavaScript's `Math.random` function. However, these methods may not be as secure or predictable as using the `Random` module.

## See Also:

For more information on the `Random` module and other useful programming concepts in Elm, check out the official Elm Guide (https://guide.elm-lang.org) and the `Random` module documentation (https://package.elm-lang.org/packages/elm/core/latest/Random).
---
title:    "Elm recipe: Generating random numbers"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers may seem like a trivial task in programming, but it actually has a variety of practical applications. Whether you need to simulate a dice roll in a game or generate a unique user ID for a web application, having the ability to create random numbers can be a useful tool in your programming arsenal.

## How To

Luckily, in Elm, generating random numbers is a simple and straightforward process. Let's take a look at a few examples using the `Random` module.

To generate a random integer between 1 and 10, we can use the `int` function and specify the range:

```Elm
import Random

Random.int 1 10
```

This will return a `Random.Generator Int` type, which can be used in various ways depending on your application. To actually get the random number, we can use the `generate` function and provide a seed value (which can be any number):

```Elm
import Random

Random.generate (Random.int 1 10) 123
```

This will return a `Result` type, which we can then handle in our code to retrieve the random number.

For more complex cases, we can also generate random floats, booleans, and even lists of values using the `float`, `bool`, and `list` functions respectively. By utilizing these functions and providing different ranges and seed values, we can generate a wide range of random numbers to suit our needs.

## Deep Dive

To understand how generating random numbers works in Elm, we need to take a closer look at the `Random` module. This module relies on a concept called pseudo-randomness, which uses algorithms to produce unpredictable sequences of numbers. Specifically, Elm uses the Mersenne Twister algorithm to generate random numbers.

Another important aspect to consider is the use of seed values. In programming, a seed is an initial value that is used as a starting point for generating random numbers. The same seed will always produce the same sequence of random numbers. This is helpful for debugging and testing purposes, as we can provide a constant seed for consistent results.

However, if we want truly random numbers, we can use `Time.now` as our seed value. This function will use the current time as the seed, resulting in a new sequence of random numbers each time the code is run.

## See Also

- Official Elm documentation on the `Random` module: https://package.elm-lang.org/packages/elm/random/latest/
- A tutorial on using random numbers in Elm: https://elmprogramming.com/random-numbers.html
---
title:                "Generating random numbers"
html_title:           "Gleam recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is a common task in programming, where a program needs to generate a series of numbers that appear to be chosen randomly. This can be useful for simulations, games, and encryption. Programmers use random numbers to add unpredictability and variety to their programs.

## How to:

To generate a random number in Gleam, we can use the `random` module, specifically the `int` function. This function takes in a minimum and maximum value, and returns a random integer between those two values. For example, ```Gleam
random.int(1, 10)
``` 
would produce a random number between 1 and 10. 

If we want to generate a random floating-point number, we can use the `float` function in the `random` module. This function also takes in a minimum and maximum value, and returns a random decimal number between them. For example, ```Gleam
random.float(0.0, 1.0)
```
would generate a random number between 0.0 and 1.0.

We can also use the `bool` function in the `random` module to generate a random boolean value. This function takes no arguments and returns either `true` or `false`. For example, ```Gleam
random.bool()
```
would generate either `true` or `false` at random.

## Deep Dive:

Random number generation has been around since the early days of computing, with early algorithms using equations and patterns to generate seemingly random numbers. However, these algorithms were eventually found to not be completely random, leading to the development of more robust and cryptographically secure random number generators. Gleam's `random` module uses the Mersenne-Twister algorithm, which is a widely used and trustworthy method for generating random numbers.

While Gleam's `random` module is a convenient way to generate random numbers, there are other approaches such as using external sources of randomness like physical dice or coin flips. Additionally, some programming languages have built-in functions for generating random numbers, but Gleam's `random` module offers more control and versatility in choosing the range and type of numbers.

## See Also:

To learn more about Gleam's `random` module and its functions, check out the official documentation here: https://gleam.run/modules/random. You can also explore other modules and features of Gleam on the official website: https://gleam.run/.
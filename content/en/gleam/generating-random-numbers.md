---
title:                "Gleam recipe: Generating random numbers"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Random numbers are a fundamental aspect of programming, used in a variety of applications such as simulations, games, and cryptography. By learning how to generate random numbers in Gleam, you can add a new level of randomness and unpredictability to your code.

## How To

```Gleam
import gleam/random

// Generate a random integer between 0 and 10
let number = random.int(0, 10)
// Output: 8

// Generate a random float between 0 and 1
let float = random.float()
// Output: 0.6539969942797535

// Generate a list of 5 random boolean values
let list = random.bools(5)
// Output: [true, true, false, false, true]
```

## Deep Dive
There are different methods and algorithms for generating random numbers, but it's important to note that computers are not truly able to generate completely random numbers. Instead, they use mathematical formulas to produce a sequence of numbers that appear random.

In Gleam, the `random` module provides functions for generating integers, floats, booleans, and even complex types like tuples and records. These functions use a default generator based on the Mersenne Twister algorithm, but you can also specify a custom generator for more control over the randomness.

It's also worth mentioning that the `random` module is based on the `rng` library, which provides a secure and cryptographically strong random number generator. This is important for applications that require a higher level of randomness and security.

## See Also
- [Gleam documentation on generating random numbers](https://gleam.run/modules/gleam_random/latest/random.html)
- [Mersenne Twister algorithm](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Secure random number generation in Gleam](https://gleam.run/news/secure-random-numbers-in-gleam)
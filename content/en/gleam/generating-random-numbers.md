---
title:                "Generating random numbers"
date:                  2024-01-27T20:26:16.068379-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in programming can be critical for creating simulations, testing, cryptography, and games. In Gleam, it's a feature that lets developers introduce unpredictability or simulate real-world scenarios in their applications.

## How to:

To generate random numbers in Gleam, you primarily use the `gleam_random` library. This library provides functions to generate random integers, floats, and more. First, ensure you've added `gleam_random` to your `rebar.config` or `mix.exs` file as a dependency.

Let's dive into some examples:

### Generating a Random Integer

To produce a random integer within a specified range, you can use the `int` function:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

This function will generate a random integer between 1 and 10 inclusive.

### Generating a Random Float

To get a random float, use the `float` function. This generates a float between 0.0 and 1.0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Example Output

Running these functions might yield outputs such as:

- For `generate_random_int()`: `5`
- For `generate_random_float()`: `0.84372`

Remember, each execution could lead to different outputs due to the nature of randomness.

## Deep Dive

The `gleam_random` module implements a pseudorandom number generator (PRNG), which essentially means the numbers aren't truly random but are difficult to predict, emulating randomness. PRNGs operate by starting with an initial value, known as the seed, and applying mathematical operations to generate a sequence of numbers.

Historically, languages and libraries have implemented several algorithms for PRNGs, like the Mersenne Twister or Linear Congruential Generator (LCG). The choice of algorithm impacts the quality of the "randomness," with some being more suitable for cryptographic applications than others. While Gleam's standard library provides convenience and ease of use with its `gleam_random` module, it may not always be the best choice for use cases requiring cryptographically secure randomness. For cryptographic purposes, developers should look into libraries specifically designed to provide cryptographically secure pseudorandom number generators (CSPRNGs), which are designed to withstand attacks that could predict future numbers based on observing a sequence of generated numbers.

In conclusion, while Gleam's random number generation functionality is robust for general programming needs, applications with specific security requirements should consider dedicated cryptographic solutions to ensure the integrity and security of their random number generation.

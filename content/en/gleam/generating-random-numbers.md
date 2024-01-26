---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:21.113042-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers means crafting unpredictable values. Programmers use them for things like testing, security, and simulating randomness in games or simulations.

## How to:
Say you're itching to make some random numbers in Gleam. Here's the skinny on how to get that done:

```gleam
import gleam/erlang
import gleam/random.{int, float}

// Generate a random integer between 1 and 10
let random_int = int(1, 10)

// Generate a random float between 0.0 and 1.0
let random_float = float()

// Outputs could be:
// random_int = 7 (remember, this will change every time!)
// random_float = 0.874563 (yup, this one too!)
```
Gleam relies on the Erlang ecosystem to manage random number generation, so keep in mind that you're tapping into Erlang's power under the hood.

## Deep Dive
Once upon a time, randomness in programming was a mystical art. Historically, generating random numbers that are truly random is hard, but modern computers can mimic randomness well enough for most purposes. In Gleam, random number generation piggybacks on Erlang's capabilities which are based on algorithms like the Mersenne Twister among others.

Alternatives outside of Gleam would include language-specific libraries, like `rand` in Rust or `random` in Python, each with their own quirks.

Under the hood, random number generation isn't truly random. It's deterministic, based on a seed. Without diving into a math lecture, know that a seed starts the random number dance. Different seeds, different dances.

Gleam inherits the robustness of Erlang's approach, which also entails dealing with concurrency and ensuring the reliable generation of random numbers in a multi-process environment. If you're playing in the Erlang ecosystem, you're standing on the shoulders of giants.

## See Also
For a broader look at random numbers in Gleam and Erlang:

- [Erlang's :rand Module](https://erlang.org/doc/man/rand.html)
- [Mersenne Twister Algorithm](https://en.wikipedia.org/wiki/Mersenne_Twister)

If you're looking to get smart on randomness and seeding:

---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers involves creating number sequences that lack any pattern. Programmers use this for tasks like creating unique identifiers, simulating unpredictable behaviour and enhancing security in applications.

## How to:
In Fish shell, generating a random number is simple. Use the built-in `random` function that has an optional range feature.

```fish
# Generate a random number between 1 and 10
random 1 10
```
This could output:
```
7
```
A random number between 1 and 10.

For a series of random numbers collectively, loop can be used:
```fish
# Generate 5 random numbers between 1 and 100
for i in (seq 5); random 1 100; end
```
This could output:
```
13
86
22
45
89
```
Each number is randomly chosen from the range of 1 to 100.

## Deep Dive
Historically, Random number generation in computing evolved from generating pseudo-random numbers where algorithms produce sequences that are statistically random. Fish Shell's `random` command, however, actually uses a cryptographic library to provide more true randomness.

Two alternatives are:
1. Pseudo-random number generator (PRNG) libraries or code snippets.
2. Using an external command-line random number generator like `shuf`.

Fish's `random` implementation works by transforming a uniformly distributed random value into a set range defined by the arguments passed. It's useful, fast, and since it's a native function of the shell, it doesn't require installing extra tools or libraries.

## See Also
For more details on how Fish shell `random` works, you can check out the official documentation here: [Fish Shell Documentation](https://fishshell.com/docs/current/commands.html#random).
For a deeper dive into random number generation, Wikipedia article is a good place to start: [Random number generation](https://en.wikipedia.org/wiki/Random_number_generation).
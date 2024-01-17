---
title:                "Generating random numbers"
html_title:           "Clojure recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is a way for programmers to create a sequence of numbers that are chosen entirely at random. This may be useful in a variety of situations, such as in gaming applications, cryptography, or simulations.

## How to:

Clojure provides a built-in function for generating random numbers called `rand`. It takes a single argument `n` which specifies the upper bound (exclusive) of the range of numbers to be generated.

```
Clojure (rand n) ;returns a random number between 0 (inclusive) and n (exclusive)
```

To generate a random number between 1 and 10, we would use `(rand 10)`.

```
Clojure (rand 10) ;possible output: 7.489625
```

In addition to `rand`, Clojure also has a `rand-int` function which specifically generates whole numbers.

```
Clojure (rand-int n) ;returns a random integer between 0 (inclusive) and n (exclusive)
```

## Deep Dive:

Historically, generating random numbers was a difficult task that often relied on physical processes, such as tossing a coin or rolling dice. With the advent of computers, pseudo-random number generators (PRNGs) were developed, which use mathematical algorithms to produce seemingly random numbers.

One alternative to using built-in functions like `rand` is to use the `Random` class from the Java standard library. However, this requires more verbose code and may not be as efficient as the built-in functions in Clojure.

Clojure's `rand` function is based on the Mersenne Twister PRNG algorithm, which is considered to have a good balance between randomness and speed. It is seeded with the current time, making subsequent calls to `rand` produce different results.

## See Also:

- [Clojure docs on random numbers](https://clojuredocs.org/clojure.core/rand)
- [Explanation of PRNGs and Mersenne Twister](https://www.stat.auckland.ac.nz/~ihaka/downloads/lexical.pdf)
---
title:                "Go recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Generating random numbers is an essential aspect of many programming tasks, from simulating real-life scenarios to ensuring data security. It allows for unpredictability and variation in programs, making them more realistic and secure.

## How To
To generate random numbers in Go, we can utilize the `rand` package. First, we need to import the package into our code:
```Go
import "math/rand"
```
Next, we can use the `rand.Intn()` function to generate a random integer within a specified range. For example, to generate a number between 1 and 10, we can use:
```Go
num := rand.Intn(10) + 1
```
Alternatively, we can use the `math/rand` package to generate a random float between 0.0 and 1.0:
```Go
float := rand.Float64()
```
We can also set the initial seed for our random number generator using the `rand.Seed()` function. By default, Go uses the current time as the seed, but we can also specify a seed value for reproducibility in simulations or debugging.

## Deep Dive
Under the hood, Go uses a pseudo-random number generator (PRNG) to generate random numbers. A PRNG is an algorithm that produces a sequence of numbers that appear to be random, but actually follow a deterministic pattern. This means that with the same initial state (or seed), the resulting sequence of numbers will always be the same.

Go uses the Mersenne Twister algorithm as its PRNG, which has a significantly longer period than other commonly used algorithms, meaning it will take longer for repeating patterns to appear.

To ensure true randomness, Go also has a `crypto/rand` package that uses cryptographically secure PRNGs to generate random numbers. However, this package should only be used when strong security is required, as using it can be computationally expensive.

## See Also
- [Official documentation for the rand package](https://golang.org/pkg/math/rand/)
- [Detailed explanation of pseudo-random number generators](https://www.statlect.com/fundamentals-of-probability/pseudo-random-number-generators)
- [Comparison of different PRNG algorithms](https://developer.ibm.com/technologies/systems/articles/au-prngalgorithms/)
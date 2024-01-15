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

## Why

Generating random numbers may seem like a trivial task, but it has many practical use cases. It can be used for games, simulations, cryptography, and even to test code and algorithms.

## How To

To generate a random integer between 0 and 10, we can use the `rand-int` function from the `clojure.core` library. It takes in one argument, the upper bound, and returns a random integer between 0 (inclusive) and the upper bound (exclusive).

```Clojure
(clojure.core/rand-int 10) ; output: 7
```

To generate a random decimal number between 0 and 10, we can use the `rand` function. It takes no arguments and returns a decimal number between 0 (inclusive) and 1 (exclusive).

```Clojure
(clojure.core/rand) ; output: 0.6732492319784195
```

We can also use the `rand-nth` function to generate a random element from a collection. It takes in a collection as an argument and returns a random element from that collection.

```Clojure
(def fruits ["apple" "banana" "orange"])
(clojure.core/rand-nth fruits) ; output: "orange"
```

## Deep Dive

Clojure uses a pseudo-random number generator (PRNG) to generate random numbers. This means that the numbers are not truly random, but they are generated using an algorithm that produces a sequence of numbers that appear to be random.

The PRNG in Clojure is based on the Mersenne Twister algorithm, which is a widely used PRNG in many programming languages. It is highly efficient and has a very large state space, which means it can produce a massive amount of unique random numbers before repeating itself.

To ensure that the generated numbers are truly random, it is recommended to seed the PRNG with a unique value. This can be done using the `set!` function followed by the `*random-seed*` variable.

```Clojure
(set! *random-seed* 12345)
```

## See Also

- Official Clojure Documentation on Randomness: https://clojure.org/reference/randomness

- Mersenne Twister Wikipedia page: https://en.wikipedia.org/wiki/Mersenne_Twister

- Java API for random numbers: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html
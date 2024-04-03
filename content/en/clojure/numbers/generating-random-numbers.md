---
date: 2024-01-27 20:26:17.584635-07:00
description: "How to: In Clojure, random number generation is straightforward, and\
  \ there are a couple of built-in functions that can be used right away. To generate\
  \ a\u2026"
lastmod: '2024-03-13T22:44:59.741466-06:00'
model: gpt-4-0125-preview
summary: In Clojure, random number generation is straightforward, and there are a
  couple of built-in functions that can be used right away.
title: Generating random numbers
weight: 12
---

## How to:
In Clojure, random number generation is straightforward, and there are a couple of built-in functions that can be used right away.

To generate a random floating-point number between 0 (inclusive) and 1 (exclusive), you can use the `rand` function:

```Clojure
(rand)
;; Example output: 0.7094245047062917
```

If you need an integer within a specific range, use `rand-int`:

```Clojure
(rand-int 10)
;; Example output: 7
```

This gives you a random integer between 0 (inclusive) and the number you pass as an argument (exclusive).

For generating a random number within a specific range (not limited to integers), you can combine `rand` with arithmetic:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Usage
(rand-range 10 20)
;; Example output: 14.857457734992847
```

This function `rand-range` will return a random floating-point number between the `min` and `max` values you specify.

For scenarios requiring more complex distributions or sequences of random numbers where repeatability is necessary (using seeds), you might need to look into additional libraries that extend beyond what's built-in.

## Deep Dive
The underlying mechanism for generating random numbers in most programming languages, including Clojure, typically relies on a pseudo-random number generator (PRNG). A PRNG uses an algorithm to produce a sequence of numbers that approximates the properties of random numbers. It's worth noting that because these are algorithmically generated, they are not truly random but can be sufficient for most practical purposes.

In the early days of computing, generating high-quality random numbers was a significant challenge, leading to the development of various algorithms to improve randomness and distribution. For Clojure, the built-in functions, such as `rand` and `rand-int`, are convenient for everyday use and cover a broad spectrum of common use cases.

However, for applications requiring cryptographic security or more complex statistical sampling methods, Clojure developers often turn to external libraries that offer more robust and specialized PRNGs. Libraries such as `clj-random` provide access to a wider variety of algorithms and greater control over seeding, which can be crucial for simulations, cryptographic applications, or any domain where the quality and predictability of the random number sequence could have significant implications.

While Clojure's built-in capabilities for generating random numbers are adequate for many tasks, exploring external libraries can offer deeper insights and options for tailored or more critical applications.

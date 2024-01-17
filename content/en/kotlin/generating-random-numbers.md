---
title:                "Generating random numbers"
html_title:           "Kotlin recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is the process of generating a sequence of numbers that appear to be random. Programmers often use this technique to create unpredictable and non-repetitive values for a variety of reasons. This could include creating unique identifiers, simulating real-world scenarios, or implementing algorithms that require a random input.

## How to:

```Kotlin
// Generate a random integer between 1 and 10
val randomInt = (1..10).random()

// Generate a random double between 0.0 and 1.0
val randomDouble = Math.random()

// Shuffle a list of integers in random order
val list = listOf(1, 2, 3, 4, 5)
list.shuffled()
```

Output:
```Kotlin
5 // randomInt
0.4672739049063102 // randomDouble
[3, 5, 1, 4, 2] // shuffled list
```

## Deep Dive:

The concept of generating random numbers dates back to ancient times, where people used various methods such as rolling dice or drawing from a collection of stones to create unpredictability. In programming, random numbers are typically generated using algorithms that use a seed value to create a sequence of numbers that appear random. These algorithms are known as pseudorandom number generators (PRNGs).

There are other options for generating random numbers, such as using hardware devices that measure physical phenomena like atmospheric noise. These true random number generators (TRNGs) produce a more genuinely random output but can be more expensive and less efficient.

In Kotlin, the `Random` and `Math` classes offer methods for generating random numbers. The `Random` class uses a PRNG algorithm, while the `Math` class uses a TRNG method. The choice between the two depends on the level of randomness required for the application.

## See Also:

- [Kotlin Random documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Math.random() documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--)
- [True random number generation](https://www.random.org/randomness/)
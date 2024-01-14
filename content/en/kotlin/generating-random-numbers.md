---
title:                "Kotlin recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
In programming, we often need to generate random numbers for various purposes, such as creating randomized game levels or selecting a random item from a list. Thankfully, Kotlin makes it easy to generate random numbers with just a few lines of code.

## How To

In Kotlin, we can generate random numbers using the `Random` class from the `kotlin.random` package. Let's see some coding examples and their corresponding outputs:

```kotlin
// Generating a random integer between 0 (inclusive) and 10 (exclusive)
val randomNumber = Random.nextInt(10)
println(randomNumber) // Output: 5

// Generating a random double between 0 (inclusive) and 1 (exclusive)
val randomDouble = Random.nextDouble()
println(randomDouble) // Output: 0.784

// Generating a random boolean
val randomBoolean = Random.nextBoolean()
println(randomBoolean) // Output: true

// Generating a random element from a list
val fruits = listOf("apple", "banana", "orange", "grape")
val randomFruit = fruits.random()
println(randomFruit) // Output: banana
```

As we can see from the examples, the `Random` class provides useful functions for generating different types of random values. We can also use a range parameter in the `nextInt()` function to generate a random integer within a specific range.

## Deep Dive
Behind the scenes, the `Random` class uses the default Java `Random` implementation to generate random numbers. However, it also has its own algorithm for generating random integers. This algorithm uses a 48-bit seed, which is seeded from a system-dependent source of randomness when the `Random` class is created. 

In addition to the functions that were used in the coding examples, the `Random` class also provides other methods for generating random values, such as `nextBytes()`, `nextBytesFromPool()`, and `nextLong()`. These methods offer more flexibility and customization options for generating random numbers.

It is also important to note that the `Random` class is not thread-safe, meaning it should not be accessed from multiple threads at the same time. If you need to generate random numbers in a multi-threaded environment, consider using the `ThreadLocalRandom` class instead.

## See Also
To learn more about generating random numbers in Kotlin, check out these resources:

- [Random class - Kotlin Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Kotlin Random Numbers - GeeksforGeeks](https://www.geeksforgeeks.org/kotlin-random-numbers/)
- [Kotlin Random Numbers - Baeldung](https://www.baeldung.com/kotlin/random-numbers)
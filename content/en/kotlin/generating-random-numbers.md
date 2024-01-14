---
title:    "Kotlin recipe: Generating random numbers"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to generate random numbers in your programming projects? Whether it's for a game, data simulation, or testing purposes, generating random numbers is a common task in many programming languages. In Kotlin, there are multiple ways to generate random numbers, each with its own advantages and use cases. In this blog post, we'll explore the different methods of generating random numbers in Kotlin and when to use them.

## How To

Kotlin provides a few different ways to generate random numbers. Here are three of the most commonly used methods:

#### 1. Using the `random` method

The simplest way to generate random numbers in Kotlin is by using the `random` method from the `kotlin.random` package. This method returns a random `Int` value between 0 and the provided argument.

```
val randomNumber = random.nextInt(10)
println(randomNumber)
```

Output: `8`

#### 2. Using `Random` class

Another way to generate random numbers is by using the `Random` class from the `java.util` package. This class provides various methods to generate random numbers of different types, such as `nextInt()`, `nextFloat()`, `nextBoolean()`, etc.

```
val random = Random()
val randomNumber = random.nextInt(100)
println(randomNumber)
```

Output: `35`

#### 3. Using `ThreadLocalRandom` class

If you're working with multi-threaded applications, it's recommended to use the `ThreadLocalRandom` class from the `java.util.concurrent` package. This class provides thread-local random number generators that generate random numbers more efficiently in a multi-threaded environment.

```
val random = ThreadLocalRandom.current()
val randomNumber = random.nextInt(1000)
println(randomNumber)
```

Output: `789`

## Deep Dive

The choice between the different ways of generating random numbers in Kotlin depends on the specific use case. The `random` method is easy to use and sufficient for most cases. However, it has the disadvantage of generating numbers that are not truly random as it uses a pseudo-random algorithm. If true randomness is crucial, it's recommended to use the `Random` or `ThreadLocalRandom` class, which both use a cryptographically secure pseudo-random number generator.

Additionally, the `Random` class allows you to specify a seed value, making it possible to generate the same series of random numbers each time. This can be useful for testing purposes.

## See Also

- [KotlinDocs on Generating Random Numbers](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)
- [JavaDocs on Random Class](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [JavaDocs on ThreadLocalRandom Class](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)
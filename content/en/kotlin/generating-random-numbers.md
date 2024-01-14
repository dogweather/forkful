---
title:    "Kotlin recipe: Generating random numbers"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Random numbers are an important aspect of many programs, from simple games to complex simulations. They add an element of unpredictability and can be used in a variety of ways. In this post, we will explore how to generate random numbers in Kotlin and why it is useful.

## How To

Generating a random number in Kotlin is easy with the `Random` class. First, we need to import the class:

```Kotlin
import kotlin.random.Random
```

Next, we can use the `nextInt()` function to generate a random integer within a specific range. For example, if we want a random number between 1 and 10, we can use the following code:

```Kotlin
val randomNumber = Random.nextInt(1, 11)
```

The `nextInt()` function takes in two arguments - the lower bound (inclusive) and the upper bound (exclusive). So in this case, we are generating a random number between 1 and 10.

We can also use the `nextDouble()` function to generate a random double between 0.0 (inclusive) and 1.0 (exclusive).

```Kotlin
val randomDouble = Random.nextDouble()
```

If we want a random boolean value, we can use the `nextBoolean()` function.

```Kotlin
val randomBoolean = Random.nextBoolean()
```

We can also generate a random number within a specific range but with a specific step using the `nextInt(range, step)` function. For example, if we want a random number between 0 and 100 but with a step of 5, we can use the following code:

```Kotlin
val randomNumber = Random.nextInt(0, 101, 5)
```

## Deep Dive

Behind the scenes, the `Random` class uses a mathematical algorithm known as a pseudorandom number generator (PRNG) to generate the random numbers. This means that the numbers are not truly random, but they have a good enough randomness for most use cases.

It is important to note that Kotlin's `Random` class is not thread-safe, meaning that it should not be used for generating random numbers in multi-threaded environments.

Additionally, if you need a more secure and truly random number generator, Kotlin also has the `SecureRandom` class which uses a cryptographic PRNG.

## See Also

For more information on generating random numbers in Kotlin, check out these resources:

- [Kotlin documentation on Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/random/-random/)
- [Kotlin documentation on SecureRandom](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/random/-secure-random/)
- [Java documentation on Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java documentation on SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
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

## Why

Creating random numbers is a common task in programming and can serve various purposes such as generating test data, simulating real-life scenarios, or implementing random elements in games or simulations.

## How To

Generating random numbers in Kotlin is made simple with the use of the `kotlin.random` package. Let's take a look at two common ways of generating random numbers in Kotlin.

### Generating a Random Integer

To generate a random integer within a specified range, we can use the `Random.nextInt()` function. This function takes in two parameters, `start` and `end`, which define the range from which the random number will be generated. For example, to generate a random number between 1 and 10, we can use the following code:

```Kotlin
val randomInt = Random.nextInt(1, 11)
println(randomInt)
```

This will output a random integer between 1 (inclusive) and 11 (exclusive), such as 5 or 10.

### Generating a Random Double

To generate a random decimal number, we can use the `Random.nextDouble()` function. This function takes in a single parameter, `range`, which defines the upper bound for the random number. For example, to generate a random number between 0 and 1, we can use the following code:

```Kotlin
val randomDouble = Random.nextDouble(1.0)
println(randomDouble)
```

This will output a random double between 0 (inclusive) and 1 (exclusive), such as 0.25 or 0.99.

## Deep Dive

Kotlin provides various functions for generating different types of random numbers, such as `nextLong()`, `nextFloat()`, and `nextBytes()`. Additionally, we can also set a seed value for the random number generator using the `Random(seed)` constructor. This is helpful for generating a specific set of random numbers consistently.

Another key aspect to consider when working with random numbers is their distribution. The `Random` class uses a uniform distribution, which means that all numbers in the given range have an equal chance of being generated. However, if we want to achieve a different distribution, such as a normal distribution, we can use the `Random.nextDouble()` function in conjunction with a mathematical formula, such as the Box-Muller transform, to transform the uniformly distributed random number into the desired distribution.

## See Also

- Official documentation for the `kotlin.random` package: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html
- Tutorial on generating random numbers in Kotlin: https://www.tutorialkart.com/kotlin/generate-random-number-kotlin/
- Article on creating a custom distribution for random numbers in Kotlin: https://kotlinexpertise.com/kotlin-random-number-generator/
---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:18.085752-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Random numbers are the backbone of unpredictability in coding – think games, simulations, and security. We use them to make things less predictable and more dynamic.

## How to:
In Kotlin, getting a random number is like grabbing a snack from the fridge – pretty straightforward. Here's how:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Random number between 1 and 99
    println(randomNumber)
}
```
Output: (this will vary every time you run it)

```37``` (or some other random number)

Need a random double or a nifty list shuffle? Kotlin's got your back:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(0.0, 10.0) // Random double between 0.0 and 10.0
    println(randomDouble)

    val myList = mutableListOf("apple", "banana", "cherry")
    myList.shuffle()
    println(myList)
}
```
Output:

```8.674523233351658``` (random double, will vary)

```[banana, cherry, apple]``` (shuffled list, will vary)

## Deep Dive
Random number generation dates way back. Computers aren't great at being random, so they use algorithms to mimic randomness –pseudo-randomness.

Kotlin uses `java.util.Random` under the hood but wraps it in a nicer package. `Random.nextInt(range)` and friends are Kotlin's way of making random numbers easy to get. You've got a whole candy store of options: integers, doubles, floats, booleans, and even shuffles and samples froom collections.

These methods use a seed to start the random sequence. Same seed, same sequence – like a "choose your own adventure" book. Without a seed, Kotlin uses the current time or another source to avoid repeats.

Before Kotlin, we'd use `Math.random()` in Java or wrangle with more complex libraries. Kotlin's random extensions are more cohesive and idiomatic, though you could still pull out the old Java way if you really wanted to.

## See Also
Dive deeper into the Kotlin random API with these links:
- [Kotlin Standard Library: Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Kotlin Documentation: Sample Random Generations](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/)

---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers in code is like rolling a digital dice. It's a staple in programming to introduce an element of uncertainty or randomness, allowing for a myriad of applications, from simulating unpredictable behaviors in games to securing data in encryption systems.

## How to:
It's pretty straightforward in Kotlin, starting with the basics:

```Kotlin
val randomInt = Random.nextInt() // Get a random Integer
println(randomInt)
```

Run it and you'll get a random number, including the negatives.

For a range of numbers? Here you go.

```Kotlin
val randomInRange = Random.nextInt(1, 100) // Get random Integer between 1 and 100
println(randomInRange)
```

Just change the range to suit what you want. Run it and you'll get a random number between 1 and 100.

## Deep Dive
The Kotlin Random library has roots in ancient history. The concept of generating random numbers computationally dates back to the 1940s and has evolved over time, yielding a variety of methods. 

For alternatives? There's Java's `Random` or `ThreadLocalRandom`. They have more utility methods but Kotlin's `Random` is simple and does the job.

Under the hood, Kotlin's `Random` uses the "xorshift" algorithm, known for its simplicity and speed. This algorithm generates pseudo-random numbers - calculated, but hard to predict.

*Fun Fact!* It's mathematically impossible to generate a truly random number computationally. Everything on a computer follows strict, predictable laws. The "random" numbers we get are "good enough" for most purposes.

## See Also
Check out these resources on random numbers in Kotlin:

- [Kotlin official docs on Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [xorshift on Wikipedia](https://en.wikipedia.org/wiki/Xorshift)
- [Java's Random library](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/Random.html) for comparison
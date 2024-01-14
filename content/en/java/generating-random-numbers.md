---
title:    "Java recipe: Generating random numbers"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why
Generating random numbers is a crucial task in software development. It allows us to add unpredictability and variability to our programs, making them more dynamic and realistic. This can be useful in simulations, games, and even security applications.

## How To
In Java, we can use the `Random` class to generate random numbers. First, we need to import the `java.util.Random` package. Then, we can create an instance of the `Random` class:

```java
Random random = new Random();
```

To generate a random integer, we can use the `nextInt()` method:

```java
int num = random.nextInt();
```

We can also specify the range for the random number to be generated. For example, to generate a random integer between 1 and 100:

```java
int num = random.nextInt(100) + 1;
```

To generate a random double, we can use the `nextDouble()` method:

```java
double num = random.nextDouble();
```

We can also generate a random boolean value using the `nextBoolean()` method:

```java
boolean value = random.nextBoolean();
```

## Deep Dive
Although the `Random` class may seem simple, it is actually based on a complex mathematical algorithm known as a pseudo-random number generator. This algorithm generates a sequence of numbers that appear to be random, but in reality, they follow a deterministic pattern.

It is worth noting that the `Random` class is not truly random since the generated values can be reproduced if we know the algorithm and the initial value (also known as the seed). To set a specific seed value, we can use the `setSeed()` method:

```java
random.setSeed(1234);
```

Another important aspect to consider is that the `Random` class can only generate pseudo-random numbers, which means that the generated values are not truly random and can eventually repeat. To deal with this issue, we can use a larger seed value or combine multiple random values to create more unique and random results.

## See Also
- [Random class in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Generating random numbers in Java](https://www.baeldung.com/java-generating-random-numbers)
- [Pseudo-random number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
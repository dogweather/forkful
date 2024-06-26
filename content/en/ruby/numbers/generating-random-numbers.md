---
date: 2024-01-27 20:26:06.703048-07:00
description: 'How to: Ruby provides several methods to generate random numbers, primarily
  via the `Random` class.'
lastmod: '2024-04-05T21:53:36.252409-06:00'
model: gpt-4-0125-preview
summary: Ruby provides several methods to generate random numbers, primarily via the
  `Random` class.
title: Generating random numbers
weight: 12
---

## How to:
Ruby provides several methods to generate random numbers, primarily via the `Random` class.

### Basic Random Number
To generate a basic random number:

```Ruby
puts rand(10) # Generates a random number between 0 and 9
```

### Random Number Within a Range
For a random number within a specific range:

```Ruby
puts rand(1..10) # Generates a random number between 1 and 10
```

### Using the Random Class
To create a repeatable sequence of random numbers, you can use the `Random` class with a seed.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # Generates a predictable "random" number
```

### Generating a Random Array Element
Select a random element from an array:

```Ruby
colors = ["red", "blue", "green", "yellow"]
puts colors.sample # Randomly selects an element from the array
```

### Sample Output:
Each code snippet above, when run, will produce different outputs due to their random nature. For example, `rand(10)` might output `7`, while `colors.sample` might output `"green"`.

## Deep Dive
The concept of generating random numbers in computer science is paradoxical because computers follow deterministic instructions. Early methods depended heavily on external input to achieve unpredictability. Ruby's randomness is built on the Mersenne Twister algorithm, a pseudo-random number generator known for its vast period and uniform distribution, making it highly suitable for applications requiring high-quality randomness.

While Ruby's built-in methods serve most needs well, they might not suffice for all cryptographic purposes, as the predictability of pseudo-random numbers can be a vulnerability. For cryptographic security, Ruby developers might explore libraries like `OpenSSL::Random`, which are designed to produce cryptographically secure random numbers, ensuring higher unpredictability for sensitive applications.

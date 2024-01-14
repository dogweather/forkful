---
title:                "TypeScript recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often come across scenarios where we need to generate random numbers. Whether it's for creating randomized user IDs, implementing game mechanics, or simply for testing purposes, the ability to generate random numbers is a useful skill to have. In this blog post, we'll dive into some ways to generate random numbers in TypeScript.

## How To

Generating random numbers in TypeScript is fairly straightforward. There are a few different methods that we can use depending on our needs.

### Using Math.random()

The simplest way to generate random numbers in TypeScript is to use the built-in Math.random() method. This method returns a random number between 0 (inclusive) and 1 (exclusive). We can use this to generate a random decimal number by multiplying the result with our desired range and then rounding it to the nearest integer.

```TypeScript
const randomNumber = Math.round(Math.random() * 10); // generates a random number between 0 and 10
```

### Using Math.floor()

If we want to generate a random integer instead, we can use the Math.floor() method in combination with Math.random(). This method rounds the decimal number down to the nearest integer, giving us a more even distribution of our random numbers.

```TypeScript
const randomNumber = Math.floor(Math.random() * 10); // generates a random integer between 0 and 9
```

### Using custom functions

We can also create our own custom functions for generating random numbers in TypeScript. For example, if we wanted to generate a random number within a specific range, we could create a function that takes in a minimum and maximum value and returns a random number in between them.

```TypeScript
function getRandomNumber(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

const randomNumber = getRandomNumber(5, 10); // generates a random number between 5 and 10
```

Keep in mind that these methods will not generate truly random numbers, as the results are still based on mathematical algorithms. However, they are often sufficient for most use cases.

## Deep Dive

If you're interested in learning more about the concept of randomness and generating truly random numbers in TypeScript, there are several resources available online that dive deeper into this topic. Some interesting topics to explore are pseudo-random number generators, the concept of seed values, and implementing randomness in computer simulations.

## See Also

- [TypeScript Math - Random](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-6.html#-math-random-changes)
- [Generating Random Numbers in TypeScript](https://thecodebarbarian.com/generating-random-numbers-in-typescript)
- [Exploring Randomness in TypeScript](https://dev.to/hulyakarakaya_/exploring-randomness-in-typescript-3i8p)
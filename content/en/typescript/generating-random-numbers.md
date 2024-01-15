---
title:                "Generating random numbers"
html_title:           "TypeScript recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Randomness is an important aspect of many applications and games. Generating random numbers allows for unpredictable results, which adds an element of surprise and variety to these programs. This can improve overall user experience and make applications more engaging.

## How To

Generating random numbers in TypeScript is a simple task that can be done using the `Math.random()` function. This function returns a random number between 0 (inclusive) and 1 (exclusive). Let's take a look at an example:

```typescript
// Generate a random number between 1 and 10
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // Output: 7 (this will be a different value each time)
```

In the above code, we use the `Math.random()` function to generate a random decimal number between 0 and 1. We then multiply it by 10 to get a range of 0 to 9.999 (since the maximum value of `Math.random()` is 0.999). Finally, we use `Math.floor()` to round down to the nearest whole number and add 1 to get a range of 1 to 10.

You can also use `Math.ceil()` to round up to the nearest whole number, or `Math.round()` to round to the nearest whole number depending on your needs.

## Deep Dive

Behind the scenes, the `Math.random()` function uses a pseudo-random number generator (PRNG) to generate the random numbers. This means that the numbers are not truly random, but are determined by a mathematical algorithm. As a result, the sequence of numbers produced by `Math.random()` is predictable and not truly random.

Also, keep in mind that the `Math.random()` function is only suitable for generating numbers with a uniform distribution. This means that each number has an equal chance of being generated. If you need a different distribution, you may have to use a different method or algorithm.

## See Also

Here are some helpful resources to learn more about generating random numbers in TypeScript:

- [MDN Docs: Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [TypeScript Official Website](https://www.typescriptlang.org/)
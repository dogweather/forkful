---
title:                "TypeScript recipe: Generating random numbers"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a crucial aspect of programming, especially when dealing with any sort of randomization or randomness in calculations. It allows for a more dynamic and unpredictable program, which can be useful in various applications such as games, simulations, and statistical analysis.

## How To

Generating random numbers in TypeScript is a straightforward process thanks to the built-in `Math.random()` method. This method returns a decimal value between 0 (inclusive) and 1 (exclusive). We can use this to our advantage by multiplying this value by the desired range of our random number and then adding the minimum value. Let's take a look at an example:

```TypeScript
// Generate a random number between 1 and 10
const randomNum = Math.random() * 10 + 1;
console.log(randomNum); // Output: 7.88987632
```

In the above example, we first multiplied the `Math.random()` value by 10 to get a range of 0 to 9.9999... and then added 1 to that value to get a range of 1 to 10. We can easily modify this code to generate a random number within any desired range.

Additionally, if we want to get an integer value instead of a decimal, we can use the `Math.floor()` method to round down our random number. Here's an example:

```TypeScript
// Generate a random integer between 1 and 10
const randomInt = Math.floor(Math.random() * 10 + 1);
console.log(randomInt); // Output: 7
```

## Deep Dive

While `Math.random()` is a simple and effective way to generate random numbers, it has some limitations. Firstly, it only generates numbers within the range of 0 to 1. This can be limiting when we want to generate numbers outside of this range. In such cases, we can use the `Math.random()` method in combination with other mathematical operations or functions to get our desired range.

Another limitation of `Math.random()` is that it is not truly random, as it follows a specific algorithm to generate numbers. For applications requiring a higher level of randomness, we can use external libraries, such as `random-js` or `seed-random`, to generate better-quality random numbers.

## See Also

For more information on generating random numbers in TypeScript, check out these resources:

- [MDN Web Docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [TypeScript Deep Dive - Randomness](https://basarat.gitbooks.io/typescript/docs/types/type-system.html#random)
- [Random.js Library](https://www.npmjs.com/package/random-js)
- [Seed-Random Library](https://www.npmjs.com/package/seed-random)
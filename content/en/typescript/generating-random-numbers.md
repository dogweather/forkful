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

## What & Why? 
Generating random numbers is a common task in programming which involves generating a random number within a given range or set of numbers. Programmers use this technique for various reasons, such as creating randomized elements in games, generating unique identifiers, and more. It adds an element of unpredictability and randomness to programs, making them more interesting and versatile.

## How to:
To generate a random number in TypeScript, you can use the built-in ```Math``` object. To specify a range of numbers, you can use the ```Math.random()``` method, which returns a decimal value between 0 and 1. For example, to generate a random number between 1 and 10, you can use the following code:

```TypeScript
Math.floor(Math.random() * 10) + 1;
```

The ```Math.floor()``` function rounds the decimal value down to the nearest whole number, and by adding 1, we ensure that the final result will be between 1 and 10.

You can also use the ```Math.round()``` or ```Math.ceil()``` functions for different rounding options. And if you need a random integer instead of a decimal, you can use the ```Math.floor()``` function to round the result down to the integer value.

## Deep Dive:
The concept of generating random numbers dates back to ancient civilizations. The Chinese used the principle of yarrow sticks to generate random numbers for divination purposes, and the Greeks used dice, which is still a widely used method for random number generation today.

Besides using the ```Math``` object, another way to generate random numbers in TypeScript is by using third-party libraries such as the "random-js" library, which provides more advanced features and methods for generating random numbers.

When it comes to implementation details, generating truly random numbers in a computer program is technically not possible as computers are inherently deterministic machines. However, various algorithms and techniques such as the Linear Congruential Generator (LCG) and the Mersenne Twister algorithm are commonly used to approximate randomness.

## See Also:
- [MDN Web Docs: Math.random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Random-js library](https://www.npmjs.com/package/random-js)
- [History of random numbers](https://www.random.org/randomness/)
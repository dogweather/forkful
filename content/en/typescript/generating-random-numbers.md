---
title:    "TypeScript recipe: Generating random numbers"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

Generating random numbers is a fundamental part of programming, and it can be incredibly useful in various applications. Whether you need to simulate a game, generate unique identifiers, or shuffle data, having a way to create random numbers is essential.

## How To

Generating random numbers in TypeScript is simple and can be done using the built-in `Math.random()` function. This function returns a floating-point number between 0 and 1, which can then be manipulated to fit your specific needs.

To generate a random whole number within a specific range, you can use the `Math.floor()` and `Math.ceil()` functions to round down or up, respectively. For example:

```TypeScript
let randomNumber = Math.random(); // returns a random number between 0 and 1
let randomInteger = Math.floor(randomNumber * 10); // returns a random whole number between 0 and 9
```

You can also use `Math.round()` to get a random integer within a specific range, such as between 1 and 10:

```TypeScript
let randomInteger = Math.round(Math.random() * 9) + 1; // returns a random whole number between 1 and 10
```

To generate a random number with decimal places, you can use `toFixed()` to specify the desired number of decimal places. For example:

```TypeScript
let randomDecimal = Math.random().toFixed(2); // returns a random number with 2 decimal places
```

## Deep Dive

If you want to dive deeper into random numbers, you can explore other options besides using `Math.random()`. For example, there is the `crypto.getRandomValues()` function, which uses a cryptographic algorithm to generate a more secure random number.

You can also create your own random number generator by implementing algorithms like the Middle-Square Method or the Linear Congruential Generator. These algorithms may require more coding and understanding of mathematical concepts, but they give you more control over the randomness of your numbers.

It's also essential to understand that computers can only generate pseudorandom numbers, which means they are not truly random but follow predictable patterns. However, these are usually sufficient for most use cases.

## See Also

- [Math.random() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Generating random numbers in TypeScript - StackOverflow](https://stackoverflow.com/questions/41786966/generating-random-numbers-in-typescript)
- [Generating random numbers in TypeScript - Medium](https://medium.com/@luistelensky/generating-random-numbers-in-typescript-961c8e6d3901)
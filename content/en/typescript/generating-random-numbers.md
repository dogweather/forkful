---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:05.238279-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in TypeScript is about creating values that are unpredictable and do not follow a discernible pattern. Programmers use them for a variety of reasons, such as simulating real-world phenomena, generating unique identifiers, or adding unpredictability to games and security algorithms.

## How to:

In TypeScript, generating a simple random number can be achieved using the `Math.random()` function, which returns a floating-point, pseudo-random number in the range 0 to less than 1 (inclusive of 0, but not 1). Here’s how to use it:

```TypeScript
// Generating a basic random number
const randomNumber = Math.random();
console.log(randomNumber); // Example output: 0.123456
```

To make this random number more useful, you might want to scale it to a different range or convert it into an integer. For instance, generating a random integer between two values, inclusive:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1) + min); //The maximum is inclusive and the minimum is inclusive
}

console.log(getRandomInt(1, 100)); // Example output: 45
```

The above function `getRandomInt` takes two arguments, `min` and `max`, and returns a random integer within and including both.

## Deep Dive

The `Math.random()` function, which is native to JavaScript and used in TypeScript, does not generate truly random numbers but pseudo-random numbers using a mathematical formula. For most casual uses, such as simple games or generating mock-up data, this level of randomness is sufficient. However, it's worth noting that since these numbers are algorithmically produced, they should not be used for cryptographic purposes where true randomness is required.

For applications needing cryptographic security, consider using the Web Crypto API. This API provides cryptographic functionalities including a more secure source of random values:

```TypeScript
async function getCryptoRandom() {
  if (window.crypto && window.crypto.getRandomValues) {
    const buffer = new Uint32Array(1);
    window.crypto.getRandomValues(buffer);
    return buffer[0];
  } else {
    throw new Error("Current environment does not support crypto API");
  }
}

getCryptoRandom().then(randomNumber => console.log(randomNumber)); // Example output: 1034871203
```

This method leverages the platform's cryptographic primitives to generate a random number, offering a stronger guarantee of unpredictability, making it suitable for security-sensitive applications.

While TypeScript itself does not alter or extend the capabilities for generating random numbers beyond what JavaScript offers, understanding these utilities and their appropriate use-cases is essential for developing robust and secure applications.

## See also

### TypeScript Official Documentation
- [Math in TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-2.html#bigint)

### Tutorials and Guides
- **Stack Overflow**: [Generating random numbers in TypeScript](https://stackoverflow.com/questions/1527803/generating-random-whole-numbers-in-javascript-in-a-specific-range)
- **Medium**: [How to Generate Random Numbers in TypeScript](https://medium.com/dailyjs/how-to-generate-random-numbers-in-javascript-3230e93fee3a)

### Online Courses and Videos
- **YouTube**: [Random Numbers in TypeScript](https://www.youtube.com/watch?v=s4DlWSlyGvA)

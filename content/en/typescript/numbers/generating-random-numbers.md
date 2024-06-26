---
date: 2024-01-27 20:26:54.398270-07:00
description: "How to: In TypeScript, you can generate random numbers using the global\
  \ `Math` object. Below are some practical examples demonstrating how to produce\u2026"
lastmod: '2024-03-13T22:44:59.855079-06:00'
model: gpt-4-0125-preview
summary: In TypeScript, you can generate random numbers using the global `Math` object.
title: Generating random numbers
weight: 12
---

## How to:
In TypeScript, you can generate random numbers using the global `Math` object. Below are some practical examples demonstrating how to produce random numbers for different requirements.

### Generating a Basic Random Number
To generate a basic random decimal number between 0 (inclusive) and 1 (exclusive), you use `Math.random()`. This does not require any additional manipulation:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

This might output a value like `0.8995452185604771`.

### Generating a Random Integer Between Two Values
When you need an integer between two specific values, you incorporate both `Math.random()` and some arithmetic:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

This might output an integer value between 1 and 10, such as `7`.

### Generating a Unique Identifier
Random numbers can be combined with other methods to create unique identifiers, for instance, a simple UUID generator snippet:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

This generates a string resembling a UUID, such as `110e8400-e29b-41d4-a716-446655440000`.

## Deep Dive
The primary method for generating random numbers in JavaScript and thus in TypeScript, `Math.random()`, relies on a pseudo-random number generator (PRNG). It's important to note that while the results may seem random, they are generated by a deterministic algorithm based on an initial seed value. Therefore, numbers produced by `Math.random()` are not truly random and should not be used for cryptographic purposes.

For cryptographically secure random numbers, the Web Crypto API offers `crypto.getRandomValues()`, which is accessible in environments supporting the Web Crypto standard, including modern browsers and Node.js (via the `crypto` module). Here's a quick example illustrating its use in TypeScript for generating a secure random number within a range:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

This method provides a stronger level of randomness and is more suited for security-sensitive applications. However, it's also more resource-intensive and may not be necessary for more mundane tasks, like simple simulations or non-critical random value generation.

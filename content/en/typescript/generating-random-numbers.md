---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in programming refers to the creation of numbers that lack any predictable pattern or regularity. Programmers use this for a variety of tasks, from game design to cryptography or statistical sampling.

## How to:

In TypeScript, we can generate random numbers using the 'Math' library and its 'random()' function:

```TypeScript
function getRandomInt(max: number) {
    return Math.floor(Math.random() * max);
}

console.log(getRandomInt(10));
```

This function generates a random integer between 0 and the passed argument. For instance, `getRandomInt(10)` may output 6, 4, 0, 8, etc on different runs.

## Deep Dive:

The concept of generating random numbers in programming has a rich history and some interesting technical details.

Historically, the task of generating 'real' randomness on a machine that follows deterministic principles was a big challenge. The introduction of pseudo-random generation algorithms, like the Linear Congruential Generator (LCG) or Mersenne Twister, paved the way.

There are more sophisticated ways of generating random numbers in TypeScript. For example, the crypto library in Node.js allows for Cryptographically Secure Pseudo-Random Number Generation (CSPRNG).

```TypeScript
import { randomBytes } from 'crypto';

function getRandomInt(max: number) {
  return randomBytes(1)[0] % max;
}

console.log(getRandomInt(10));
```

It's worth noting that while `Math.random()` is sufficient for most uses, CSPRNG is important for applications like cryptographic key generation, where the numbers must be unpredictable.

## See Also:

For further reading, check the following links:
- [Mozilla Docs on Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Node.js Crypto library](https://nodejs.org/api/crypto.html)
- [Linear Congruential Generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)
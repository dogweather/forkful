---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:26.311505-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers means creating numbers that are unpredictable, with no discernible pattern—think picking a lottery number. Programmers need randomness for various reasons like games, simulations, and cryptography to mimic reality or ensure security.

## How to:
JavaScript's `Math.random()` is the go-to for randomness. It generates a float between 0 (inclusive) and 1 (exclusive). Here's how to wield this tool:

### Basic Random Float:
```javascript
let randomFloat = Math.random();
console.log(randomFloat); // Outputs: 0.123456789
```

### Random Integers within a range:
Getting whole numbers between two values (e.g., min and max):

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}
console.log(getRandomInt(1, 10)); // Outputs: an integer between 1 and 10
```

## Deep Dive
`Math.random()` has been around for ages, a steady albeit imperfect randomness generator. Why imperfect? It's not truly random, it's pseudo-random, good enough for casual use but not cryptographic security. Alternates like the Web Crypto API offer more robust solutions.

### Pseudo-random vs. Cryptographically Secure:
Pseudo-random algorithms can theoretically be predicted after enough output analysis. For more secure randomness, cryptographic methods, like those in the Web Crypto API, use physical entropy sources (e.g., mouse movement) to generate unpredictable numbers.

### Historical Context:
Before `Math.random()`, randomness in programming was a tad more DIY, relying on algorithms like the middle-square method concocted by John von Neumann in the 1940s. Technology evolved, and JavaScript eventually handed us this neat little function on a silver platter.

### Implementation Details:
JavaScript's `Math.random()` uses an algorithm like Xorshift or a linear congruential generator under the hood. Fun fact: different JavaScript engines might use different algorithms, so randomness can vary slightly across browsers.

## See Also
- [Mozilla Developer Network (MDN) - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN - Web Crypto API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API)
- [Random.org's explanation of true random and pseudo-random](https://www.random.org/randomness/)

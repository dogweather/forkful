---
title:    "Javascript recipe: Generating random numbers"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how games or applications generate random numbers? Or perhaps you want to add some randomness to your own code? Whatever your reason may be, learning how to generate random numbers in Javascript can be a useful skill to have in your programming toolbox.

## How To

Generating random numbers in Javascript is surprisingly easy. All you need is the Math object's `random` method. This method will return a random number between 0 (inclusive) and 1 (exclusive). 

Let's see an example of how we can use this method:

```Javascript
// Generating a random number between 0 and 10 (inclusive)
let randomNumber = Math.random() * 10;

// Rounding the number to the nearest integer
randomNumber = Math.round(randomNumber);

console.log(randomNumber); // Outputs a random integer between 0 and 10
```

In this example, we used the `Math.round` method to round the random number to the nearest integer, giving us a more even distribution of numbers. Additionally, we multiplied the random number by 10 to get a range of 0-10 instead of 0-1.

You can also use the `Math.floor` method to round the random number down to the nearest integer, or the `Math.ceil` method to round it up. Experiment with different combinations to see which one works best for your code.

## Deep Dive

While the `Math.random` method is suitable for most cases, it is not truly random. It is known as a pseudo-random number generator, meaning that it is based on an algorithm that produces a sequence of numbers that appear to be random. 

If you require a more secure and unpredictable source of randomness, you can use the built-in `crypto` module in Node.js. This module provides access to the operating system's cryptographically strong random function, which is better suited for sensitive applications such as password generation.

Another important thing to note is that the `Math.random` method should not be used for anything related to security or encryption, as it is not truly random and can be easily predicted.

## See Also

- [MDN Web Docs: Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Node.js API: Crypto module](https://nodejs.org/api/crypto.html)
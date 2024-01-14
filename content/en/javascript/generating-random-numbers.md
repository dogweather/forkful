---
title:                "Javascript recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever played a game or used a program that required a random number to be generated? Generating random numbers is a crucial skill for any programmer, as it is often necessary for creating randomized elements in applications such as games, simulations, and security features. In this blog post, we will dive into the world of generating random numbers in Javascript and explore how it can be useful for your programming needs.

## How To

Generating random numbers in Javascript is a fairly simple process. To do this, we will be using the built-in Math object, specifically the `random()` method. This method returns a random number between 0 (inclusive) and 1 (exclusive). In order to generate numbers within a specific range, we can use a combination of `Math.random()` and some basic math operations.

Let's take a look at the syntax for generating a random integer between two given numbers:

```Javascript
// This function generates a random integer between min and max (inclusively)
function randomInt(min, max) {
  return Math.floor(min + Math.random() * (max - min + 1))
}
```

To understand how this works, let's break down the code. First, we use `Math.random()` to generate a decimal number between 0 and 1. Then, we multiply this decimal by the difference between `max` and `min` and add `min` to ensure that the number falls within the desired range. Finally, we use `Math.floor()` to round down the number to the nearest integer, which gives us our random integer between `min` and `max`.

Let's test out our function with some sample inputs:

```Javascript
// Generate a random integer between 1 and 10
console.log(randomInt(1, 10)) // Output: 7

// Generate a random integer between -100 and 100
console.log(randomInt(-100, 100)) // Output: -42
```

As you can see, we can easily generate random integers within any given range using this method. You can also use similar logic to generate decimal numbers or even strings from a set of characters.

## Deep Dive

Now that you have a basic understanding of how to generate random numbers in Javascript, let's take a deeper dive into the concept of randomness and how it applies to computer programming.

Randomness is a fundamental concept in computer science and is used in various algorithms and applications. However, true randomness is challenging to achieve in a computer program. The `Math.random()` method in Javascript uses a pseudo-random number generator, which means that the numbers it generates may appear random, but they are actually the product of a mathematical formula. This makes them predictable, hence not truly random.

To achieve a more random and unbiased outcome, you can use external sources of randomness, such as user input or data from sensors like the computer's internal clock or network traffic. However, these sources also come with their own limitations and potential biases.

## See Also

- [MDN Web Docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [W3Schools - Random Numbers in Javascript](https://www.w3schools.com/js/js_random.asp)
- [Towards Data Science - The Pitfalls of Random Number Generation in Javascript](https://towardsdatascience.com/the-pitfalls-of-random-number-generation-in-javascript-543d834c3ca5)
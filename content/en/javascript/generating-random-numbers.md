---
title:    "Javascript recipe: Generating random numbers"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why

Random numbers play a crucial role in various programming tasks such as simulations, games, cryptography, and more. It adds an element of unpredictability and removes patterns, making the program more realistic and secure. Learning how to generate random numbers in Javascript can enhance your programming skills and add a new dimension to your projects.

## How To

Generating random numbers in Javascript is a simple process. In fact, there are multiple ways to achieve it. Let's explore two different methods using built-in functions in Javascript.

#### Method 1 - Math.random()

The Math.random() function returns a random floating-point number between 0 and 1 (excluding 1). We can use this function to generate a random integer within a range by multiplying it with the maximum value and rounding it down using Math.floor().

```javascript
// Generate a random number between 1 and 10
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); //Output: 7
```

#### Method 2 - Using arrays and Math.floor()

We can also use arrays to generate a random number within a specific range. First, we create an array with all the possible values and then use the Math.floor() function to round down a random index number.

```javascript
// Generate a random number between 1 and 10
let randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(randomNumber); //Output: 4
```

## Deep Dive

Random number generation is not as straightforward as it seems. The Math.random() function, although widely used, is not completely random. It is based on an algorithm that generates a predictable sequence of numbers. There are other techniques like Pseudorandom number generators (PRNGs) and Cryptographically Secure Pseudorandom Number Generators (CSPRNGs) that are used to generate more unpredictable and secure random numbers.

PRNGs are algorithms that use a seed value to generate a sequence of numbers that look random but are actually deterministic. This means that if we use the same seed value, we will get the same sequence of numbers. On the other hand, CSPRNGs add an extra layer of security by using a combination of a seed value and external inputs such as user mouse movements or keyboard strokes.

## See Also

- [Math.random() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Pseudorandom number generation - Wikipedia](https://en.wikipedia.org/wiki/Pseudorandom_number_generation)
- [Cryptographically secure pseudorandom number generator - Wikipedia](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
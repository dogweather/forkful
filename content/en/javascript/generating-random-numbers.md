---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in JavaScript is the creation of an unpredictable number within a given range. We do it in programming to achieve variety, for example, in games, statistics, security algorithms, or simulations.

## How to:

To generate a random number in JavaScript, we use the `Math.random()` function. 

Here's a simple example where we generate a decimal number between 0 (inclusive) and 1 (exclusive):

```JavaScript
let random = Math.random();
console.log(random);
```
If you want a whole number, for example within a specified range, like 1 to 10, you can use a formula like this:

```JavaScript
let random = Math.floor(Math.random() * 10) + 1;
console.log(random);
```

## Deep Dive

Historically, JavaScript uses a pseudo-random algorithm as the basis for `Math.random()`. It's "pseudo" because it's deterministic and would generate the same sequence from the same seed value.

Alternatives to `Math.random()` include libraries such as Chance.js or crypto.getRandomValues for cryptographic purposes.

Under the hood, when you call `Math.random()`, JavaScript's runtime environment utilizes a C++ function behind the scenes to generate these 'random' numbers. This algorithm varies between runtime environments - while Node.js uses V8's implementation, browsers might use different ones.

## See Also

Check these out for more info:

1. [MDN Web Docs on `Math.random()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
2. [Details about potential pitfalls when using `Math.random()`](https://v8.dev/blog/math-random)
3. [Chance.js library](http://chancejs.com/)
4. [W3Schools guide on JavaScript Random](https://www.w3schools.com/js/js_random.asp)
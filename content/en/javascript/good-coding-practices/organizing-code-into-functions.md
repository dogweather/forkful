---
date: 2024-01-25 02:59:51.265561-07:00
description: "How to: Historically, imperative programming languages like early versions\
  \ of BASIC or Assembly lacked the abstraction that functions provide. Over time,\u2026"
lastmod: '2024-04-05T21:53:36.139686-06:00'
model: gpt-4-1106-preview
summary: Historically, imperative programming languages like early versions of BASIC
  or Assembly lacked the abstraction that functions provide.
title: Organizing code into functions
weight: 18
---

## How to:
```javascript
// Define a function to calculate the area of a rectangle
function calculateArea(width, height) {
  return width * height;
}

// Call the function and print the result
let area = calculateArea(5, 3);
console.log(area); // Output: 15
```

```javascript
// Group related functionality using functions
function greet(name) {
  console.log(`Hello, ${name}!`);
}

function farewell(name) {
  console.log(`Goodbye, ${name}!`);
}

greet('Alice'); // Output: Hello, Alice!
farewell('Bob'); // Output: Goodbye, Bob!
```

## Deep Dive
Historically, imperative programming languages like early versions of BASIC or Assembly lacked the abstraction that functions provide. Over time, the concept of modular code in languages like C introduced the idea that breaking down code into units (functions or procedures) leads to better organization and clearer logic.

In JavaScript, besides plain functions, we've got arrow functions since ES6 (2015) which provide a more concise syntax and are suited for non-method functions.

Alternatives and enhancements around organizing code in JavaScript include object-oriented approaches using classes, or functional programming paradigms that treat functions as first-class citizens.

Implementation-wise, JavaScript functions support closures, providing a way to retain access to a function's scope after execution, which is powerful for encapsulation and creating factory functions, among other patterns.

## See Also
- MDN Web Docs on Functions: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- JavaScript Design Patterns: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Clean Code JavaScript: https://github.com/ryanmcdermott/clean-code-javascript

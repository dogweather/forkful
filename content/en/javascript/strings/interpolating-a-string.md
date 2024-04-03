---
date: 2024-01-20 17:51:07.727684-07:00
description: "How to: In JavaScript, string interpolation is often done using template\
  \ literals. Here\u2019s how you can do it."
lastmod: '2024-03-13T22:45:00.420943-06:00'
model: gpt-4-1106-preview
summary: In JavaScript, string interpolation is often done using template literals.
title: Interpolating a string
weight: 8
---

## How to:
In JavaScript, string interpolation is often done using template literals. Here’s how you can do it:

```javascript
const name = 'Alice';
const message = `Hello, ${name}! How are you today?`;
console.log(message); // Outputs: Hello, Alice! How are you today?
```

You can also perform operations within placeholders:

```javascript
const a = 10;
const b = 5;
console.log(`Ten times five is ${a * b}.`); // Outputs: Ten times five is 50.
```

## Deep Dive
Historically, string interpolation wasn't as straightforward in JavaScript. Before ES6 (ECMAScript 2015), concatenation was commonly done using the `+` operator:

```javascript
var name = 'Bob';
var message = 'Hello, ' + name + '! How are you today?';
```

With the introduction of ES6, template literals (between backticks \` \`) came along, bringing an easier syntax with the `${}` placeholders.

Alternatives to string interpolation include string concatenation with the `+` operator and `concat()` method, or using `sprintf`-like functions from third-party libraries.

The performance of template literals is generally on par with these older methods. However, readability and the ability to include expressions (like `${a * b}`) within strings make template literals a strong choice for developers.

## See Also
- MDN on Template Literals: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- String concatenation in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_Operators
- A history of JavaScript module "ECMAScript": https://www.ecma-international.org/publications-and-standards/standards/ecma-262/

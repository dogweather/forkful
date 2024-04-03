---
date: 2024-01-20 17:35:31.345834-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:59.851142-06:00'
model: gpt-4-1106-preview
summary: .
title: Concatenating strings
weight: 3
---

## How to:
```TypeScript
let greeting: string = "Hello";
let target: string = "World";
let message: string = greeting + ", " + target + "!"; // using the + operator
console.log(message); // Output: Hello, World!

let anotherMessage: string = `${greeting}, ${target}!`; // using template literals
console.log(anotherMessage); // Output: Hello, World!
```

## Deep Dive
Concatenation is fundamental; it's been around since the early days of programming. In TypeScript, which builds on JavaScript, we've come a long way from clunky string operations to sleek template literals.

Historically, you had to be careful with concatenation to not use too much memory or slow down the browser. Modern engines are optimized, but efficiency still matters in large-scale apps.

There are alternatives:
1. Arrays and `.join()`: Useful when you're dealing with a list of strings.
2. StringBuilder patterns: More relevant to languages like Java or C# where it optimizes performance.

Implementation-wise, TypeScript ends up compiling to JavaScript. Under the hood, it uses the same string functions and operations provided by JavaScript.

## See Also
- You might want to check out the Mozilla Developer Network [String documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) for an in-depth look at string methods.
- For TypeScript-specific string questions, [TypeScript's official documentation](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string) is a quick reference.

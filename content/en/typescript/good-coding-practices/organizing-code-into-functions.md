---
date: 2024-01-25 03:00:11.560491-07:00
description: 'How to: Imagine you''re making a basic calculator. Instead of writing
  the addition logic everywhere you need it, create an `add` function.'
lastmod: '2024-03-13T22:44:59.863874-06:00'
model: gpt-4-1106-preview
summary: Imagine you're making a basic calculator.
title: Organizing code into functions
weight: 18
---

## How to:
Imagine you're making a basic calculator. Instead of writing the addition logic everywhere you need it, create an `add` function:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Sample output: 12
```

Now, let's say we need a function to multiply:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Sample output: 12
```
Notice how we focus on one task per function? That's the heart of organizing code.

## Deep Dive
Historically, as programming languages evolved, functions became vital in structuring code, drawing from mathematical functions. They're a staple in procedural programming and live on in object-oriented and functional programming paradigms.

Alternatives? You could just not use functions, but that's a one-way ticket to Spaghetti Town. Or you could go OOP (Object-Oriented Programming) and pack functionality into methods—which are basically functions that belong to objects.

Implementation-wise, TypeScript insists on types. Defining input and output types for functions isn't just good manners; it's a must for clean TypeScript code. Plus, with TypeScript, you get nifty features like overloads, generics, and optional parameters to supercharge your functions.

## See Also
Check out these resources to level up your function game:

- [TypeScript Handbook – Functions](https://www.typescriptlang.org/docs/handbook/2/functions.html): Your Bible for TypeScript functions.
- [Clean Code JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): Apply Clean Code principles to your JavaScript functions.
- [You Don’t Know JS – Scope & Closures](https://github.com/getify/You-Dont-Know-JS): Get a grip on how functions work with scope and closures in JavaScript.

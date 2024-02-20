---
date: 2024-01-20 17:51:48.024640-07:00
description: String interpolation lets you embed variables and expressions into strings.
  It keeps your code readable and flexible - no plus signs, no muss, no fuss.
lastmod: 2024-02-19 22:05:18.316050
model: gpt-4-1106-preview
summary: String interpolation lets you embed variables and expressions into strings.
  It keeps your code readable and flexible - no plus signs, no muss, no fuss.
title: Interpolating a string
---

{{< edit_this_page >}}

## What & Why?

String interpolation lets you embed variables and expressions into strings. It keeps your code readable and flexible - no plus signs, no muss, no fuss.

## How to:

To interpolate a string in TypeScript, you use backticks `` ` `` and `${expression}` syntax:

```TypeScript
let user = 'Charlie';
let age = 27;

// Interpolating a string
let greeting = `Hi, I'm ${user} and I'm ${age} years old.`;

console.log(greeting);  // Output: Hi, I'm Charlie and I'm 27 years old.
```

## Deep Dive:

String interpolation isn't unique to TypeScript; it's also in JavaScript since ES6 and many other languages. Before this, we concatenated strings using the `+` operator, which looked like this:

```TypeScript
let greeting = 'Hi, I\'m ' + user + ' and I\'m ' + age + ' years old.';
```

The `+` method works, but it's clunkier and harder to read, especially with multiple variables. With interpolation, templates are cleaner and errors are easier to avoid.

What's happening under the hood? Interpolated strings are "syntactic sugar"—a simplified way to use the more complex feature known as "template literals". When compiled, your friendly, readable interpolation gets converted to a format the JavaScript engine can understand, often involving concatenation or other string manipulation methods.

An alternative to interpolation would be using template functions or libraries, but for most cases, interpolation with backticks is the handiest tool for the job.

## See Also:

- [Mozilla Developer Network on Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [ES6 Features and Syntax](http://es6-features.org/#StringInterpolation)

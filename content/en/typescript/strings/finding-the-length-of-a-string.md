---
date: 2024-01-20 17:48:27.630247-07:00
description: "Finding the length of a string means measuring how many characters it\
  \ contains. Programmers do it to validate inputs, loop through characters, or limit\u2026"
lastmod: '2024-02-25T18:49:56.274655-07:00'
model: gpt-4-1106-preview
summary: "Finding the length of a string means measuring how many characters it contains.\
  \ Programmers do it to validate inputs, loop through characters, or limit\u2026"
title: Finding the length of a string
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string means measuring how many characters it contains. Programmers do it to validate inputs, loop through characters, or limit text display, among other reasons.

## How to:

In TypeScript, you get a string's length using the `.length` property. Here's a quick example:

```typescript
let greeting: string = "Hello, TypeScript!";
console.log(greeting.length); // Output: 18
```

This code declares a string variable named `greeting` and then logs its length to the console.

## Deep Dive

The `.length` property is a holdover from JavaScript, TypeScript's ancestor. It's a straightforward and universally supported way to get a string's size.

There are alternatives, but they usually complicate things. For example, you could convert the string to an array and count the elements:

```typescript
let greetingArray: string[] = Array.from(greeting);
console.log(greetingArray.length); // Output: 18
```

But why go the long way round? The `.length` property is efficient because strings are stored as character arrays under the hood, so the length information is readily available.

Now, let's say you're dealing with strings from different languages. You could run into issues with special characters. The basic `.length` approach counts UTF-16 code units, which can be problematic for characters that require two code units, known as surrogate pairs. In such cases, the `.length` property might not give you the count of actual characters, also known as code points.

Here's how you can handle strings with surrogate pairs:

```typescript
function countCodePoints(str: string): number {
    return Array.from(str).length;
}

let fancyGreeting: string = "Hello, 🌍!";
console.log(countCodePoints(fancyGreeting)); // Output: 9
```

This function deals with the intricacies of string encoding to ensure each character, regardless of whether it's a single or double code unit, is counted properly.

## See Also

- The TypeScript Handbook on Strings: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#strings)
- MDN Web Docs on the String length property: [String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Unicode and JavaScript: [JavaScript has a Unicode problem - Mathias Bynens](https://mathiasbynens.be/notes/javascript-unicode)

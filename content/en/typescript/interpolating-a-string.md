---
title:                "Interpolating a string"
html_title:           "TypeScript recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string is the process of inserting values into a string template at specific placeholders. This allows for dynamic string creation and makes the code cleaner and more readable. It also helps in avoiding lengthy string concatenation operations.

## How to:
```TypeScript
// Example 1:
const name = "John";
const age = 25;

console.log(`Hello, my name is ${name} and I am ${age} years old.`);
// Output: Hello, my name is John and I am 25 years old.

// Example 2:
function greet(name: string) {
  return `Welcome ${name}!`;
}

console.log(greet("Jane"));
// Output: Welcome Jane!
```

## Deep Dive:
Interpolating strings originated in the 1960s with the development of the first string formatting tools. However, it became more popular in the late 2000s with the introduction of template literals in JavaScript. Prior to that, string interpolation was achieved through string concatenation or using the `format()` method in formatting libraries.

An alternative to string interpolation is using string concatenation which involves joining multiple strings together using the `+` operator. However, this can become tedious and messy when dealing with longer strings or multiple variables.

In TypeScript, string interpolation is achieved by using backticks (\`...\`) instead of single or double quotes. Inside the backticks, the placeholder values are denoted by a dollar sign followed by curly braces (${...}). TypeScript also supports multi-line string interpolation, making it easier to create multi-line strings without needing to manually include the necessary escape characters.

## See Also:
- [Template literals - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [String Formatting in TypeScript - How and Why | Better Programming](https://betterprogramming.pub/string-formatting-in-typescript-how-and-why-3104696b376a)
- [TypeScript - String Interpolation | Tutorialspoint](https://www.tutorialspoint.com/typescript/typescript_string_interpolation.htm)
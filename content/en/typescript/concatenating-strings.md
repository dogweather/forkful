---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

# An Idiot-Proof Approach to String Concatenation in TypeScript

## What & Why?
Concatenating strings involve linking them together in a sequence. Why do we do it? To manipulate and assemble textual information as per the needs of our programming code.

## How to:
Putting strings together in TypeScript? Breeze! Here's how:

```TypeScript
let str1: string = "Hello, ";
let str2: string = "World!";
let greeting: string = str1.concat(str2);

console.log(greeting);  // Outputs: Hello, World!
```

A sexier, ECMAScript 2015 way to do it, is with template literals:

```TypeScript
let name: string = "World";
let greeting: string = `Hello, ${name}!`;

console.log(greeting);  // Outputs: Hello, World!
```
## Deep Dive
Concatenation has been in the game since the birth of programming. In TypeScript, we enjoy several ways of doing it.

Alternatives? Besides concat() and template literals, we've the '+' operator:

```TypeScript
let str1: string = "Hello, ";
let str2: string = "World!";
let greeting: string = str1 + str2;

console.log(greeting);  // Outputs: Hello, World!
```

Under the hood, things are simple. TypeScript uses JavaScript's underlying string methods. So, if you concatenate a number and a string, the number is converted into a string automatically.

## See Also
* TypeScript's Official Documentation: [String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
* ECMAScript 2015's [Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals) 
* A great Medium Article: [Understanding String Concatenation in TypeScript](https://medium.com/better-programming/understanding-string-concatenation-in-typescript-8c707d67c108)
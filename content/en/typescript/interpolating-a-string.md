---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Grooving with TypeScript: Interpolating a String 

## What & Why?

Interpolating a string is the process of embedding expressions within string literals, so they get evaluated as part of the string. Why do we need it? Well, it simplifies the concatenation of strings and variables—saving time and enhancing readability.

## How to:

In TypeScript, interpolation is done with backticks (`) and '${}'. Here's how:

```TypeScript
let name: string = "Anna";
let greeting: string = `Hello, ${name}!`; 
console.log(greeting); // Prints "Hello, Anna!"
```

Multiple variables? No problem. 

```TypeScript
let first: string = "Anna";
let last: string = "Johnson"
let fullName: string = `Full Name: ${first} ${last}`; 
console.log(fullName); // Prints "Full Name: Anna Johnson"
```
## Deep Dive 

String interpolation can be traced back to the early days of computer science, featuring in Perl, Ruby, and, notably, ES6 JavaScript—the prominent influencer of TypeScript. Its simplicity and readability over traditional concatenation techniques using "+", made it a favorite for developers.

Alternatives in TypeScript? You could use the concatenation operator '+'. It's verbose and less intuitive:

```TypeScript
let name: string = "Anna";
let greeting: string = "Hello, " + name + "!"; 
console.log(greeting); // Prints "Hello, Anna!"
```

Under the hood, string interpolation converts your expressions into a string and combines them with your literals. It performs similar to concatenation, with no significant performance difference.

## See Also 

2. [MDN Web Docs - Template literals (embedded expressions)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
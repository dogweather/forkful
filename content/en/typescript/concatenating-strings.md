---
title:                "Concatenating strings"
html_title:           "TypeScript recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is the act of combining two or more string values together into a single, longer string. Programmers commonly do this to create dynamic pieces of text, such as generating a unique email address or creating a personalized greeting.

## How to:

To concatenate strings in TypeScript, use the `+` operator. For example:

```TypeScript
let name = "John";
let greeting = "Hello " + name;
console.log(greeting);
```

The output would be: `Hello John`

You can also use the `concat()` method, which takes in multiple string parameters. Example:

```TypeScript
let firstName = "Jane";
let lastName = "Doe";
let fullName = firstName.concat(" ", lastName);
console.log(fullName);
```

The output would be: `Jane Doe`

## Deep Dive:

Concatenating strings may seem like a simple concept, but it has a long history in computer programming. In the early days, programmers had to manually allocate memory for each character in a string, making concatenation a tedious and error-prone task. With the advancements in programming languages and memory management, concatenation has become much simpler and faster.

Alternatives to using the `+` operator or `concat()` method include the `template literals` in TypeScript. This is a modern approach to string concatenation that allows for more complex string formatting and interpolation. Example:

```TypeScript
let age = 25;
let sentence = `I am ${age} years old.`;
console.log(sentence);
```

The output would be: `I am 25 years old.` 

## See Also:

- [TypeScript documentation on string concatenation](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Template literals in TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#template-literals)
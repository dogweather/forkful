---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Determining the length of a string means finding out how many characters it contains. It is a handy tool for programmers, enabling them to manipulate text data efficiently, be it in form validations, text parsing, truncating texts, or displaying proper messaging limits in UI.

## How to:

In TypeScript, finding the length of a string is a breeze by just using the `.length` property. Below are some examples.

```TypeScript
let text: string = "Hello, World!"

// Output the length of string
console.log(text.length); // Output: 13

let emptyStr: string = "";

// Output the length of an empty string
console.log(emptyStr.length); // Output: 0
```
Through this, you'll get respective outputs showing the length of the given string. Zero length indicates an empty string.

## Deep Dive

Historically, string length determination has been a fundamental task in programming since the birth of languages that incorporate text data manipulation, like C. In TypeScript (and JavaScript), `.length` property emerged from the need to manipulate web content.

As for alternatives, they exist but are merely overkill for such a simple task. For example, you might implement a function to iterate each character until the end of the string, and increment a counter - effectively replicating `.length`.

Under the hood, when `.length` property is called on a string, the JavaScript engine doesn't need to count the characters because it maintains an internal index on string objects. Hence, accessing `.length` property is a very fast operation.

## See Also

For further reading and more complex operations with strings in TypeScript, refer to:
- [Mozilla Developer Guide on JavaScript String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [TypeScript Handbook: Variable Declarations](https://www.typescriptlang.org/docs/handbook/variable-declarations.html)
- [ECMAScript specifications](https://www.ecma-international.org/publications/standards/Ecma-262.htm)
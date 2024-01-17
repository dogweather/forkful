---
title:                "Capitalizing a string"
html_title:           "TypeScript recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string refers to the act of making the first letter of a word uppercase, while making the rest of the letters lowercase. Programmers often do this to ensure consistency and readability in their code.

## How to:
Capitalizing a string can be easily achieved in TypeScript using the string method `toUpperCase()` and `toLowerCase()`. These methods convert a string to all uppercase or all lowercase letters. For example:

```
TypeScript
let str: string = "hello world";
console.log(str.toUpperCase()); // Outputs: HELLO WORLD
console.log(str.toLowerCase()); // Outputs: hello world
```
In this example, the `toUpperCase()` method is used to capitalize the string, while the `toLowerCase()` method is used to convert it back to lowercase.

## Deep Dive:
Capitalizing a string has been a common practice in programming since the early days. It helps to improve the readability and maintainability of code, as well as reduce errors caused by typos or inconsistencies.

There are a few alternative methods to capitalize a string in TypeScript, such as using the spread operator to split the string into an array, capitalizing the first letter, and then using the `join()` method to combine the array back into a string.

For example:
```
TypeScript
let str: string = "hello world";
let strArray: string[] = [...str];
strArray[0] = strArray[0].toUpperCase();
let capitalizedStr: string = strArray.join("");
console.log(capitalizedStr); // Outputs: Hello world
```
Additionally, there are also libraries and packages available that offer more advanced ways to capitalize strings and handle edge cases, such as strings containing special characters or multiple words.

## See Also:
- [String.prototype.toUpperCase documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.toLowerCase documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [lodash.capitalize documentation](https://lodash.com/docs/4.17.15#capitalize)
- [npm capitalize package](https://www.npmjs.com/package/capitalize)
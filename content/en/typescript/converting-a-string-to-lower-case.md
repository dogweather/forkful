---
title:                "Converting a string to lower case"
html_title:           "TypeScript recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case means changing all of the letters in a string to their lowercase equivalents. This is a common task in programming because it allows for consistency in data and makes it easier to compare strings without worrying about capitalization differences.

## How to:

### Method 1: Using the toLowerCase() method

```TypeScript
let string = "Hello World";
let lowerCaseString = string.toLowerCase();

console.log(lowerCaseString);
// Output: "hello world"
```
In this example, we use the built-in `toLowerCase()` method to convert the string to lowercase. This method will return a new string with all lowercase letters, while leaving the original string unchanged.

### Method 2: Using the spread operator and map() method

```TypeScript
let string = "Hello World";
let lowerCaseString = [...string].map((c) => c.toLowerCase()).join('');

console.log(lowerCaseString);
// Output: "hello world"
```
In this example, we use the spread operator `...` to split the string into an array of individual characters, then use the `map()` method to iterate through the array and convert each character to lowercase. Finally, we use the `join()` method to combine the characters back into a single string.

## Deep Dive:

There are a few different approaches to converting a string to lowercase, each with their own advantages and disadvantages. The `toLowerCase()` method is the simplest and most widely used, but it can be less efficient when dealing with larger strings. The spread operator and `map()` method approach provides more flexibility, but it may also be harder to read for beginner programmers.

Historically, uppercase and lowercase letters have been used in different ways in various writing systems. In the English language, capital letters were traditionally used for proper nouns and the beginning of sentences. However, in modern programming languages, capitalization is often ignored in favor of creating consistent and predictable code.

Some alternatives to the `toLowerCase()` method include `String.prototype.toLocaleLowerCase()` and `String.prototype.toLowerCase()` in JavaScript. For TypeScript, we can also use `string.toLocaleLowerCase()` or `string.toLowerCase()`, as strings in TypeScript are just extensions of strings in JavaScript.

## See Also:

- [String.prototype.toLowerCase() documentation on Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Strings in TypeScript documentation on TypeScript website](https://www.typescriptlang.org/docs/handbook/2/types-from-types.html#strings)
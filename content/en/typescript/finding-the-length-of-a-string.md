---
title:                "Finding the length of a string"
html_title:           "TypeScript recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 
Finding the length of a string means determining the number of characters in a given string. Programmers often need to find the length of a string in order to perform various operations on it, such as manipulating it, comparing it with other strings, or validating input from users.

## How to:
To find the length of a string in TypeScript, we can use the `length` property. Here's an example:

```TypeScript
const str = "Hello world";
console.log(str.length); // Output: 11
```

We store the string "Hello world" in a variable `str` and then access the `length` property to find its length. The output of this code will be `11` since there are 11 characters in the string.

## Deep Dive:
- Historical context: The concept of finding the length of a string dates back to the early days of computer programming when strings were stored as arrays of characters. The `length` property was first introduced in the programming language B, which was the predecessor of the widely used programming language C.
- Alternatives: Apart from using the `length` property, we can also use the `size` method of the `String` object. This method also returns the length of the string.
- Implementation details: The `length` property is a built-in property of the `String` object in TypeScript. It is read-only and cannot be modified by the programmer.

## See Also:
- [MDN web docs on length property](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN web docs on size method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/size)
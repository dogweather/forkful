---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lowercase in TypeScript is the process of changing all uppercase letters in a string to lowercase letters. Programmers do this to standardize data input or to implement case-insensitive comparisons.


## How To:
In TypeScript, you can convert a string to lower case using the `toLowerCase()` method. This method does not modify the original string, but returns a new string where all uppercase characters are converted to lowercase.
Here's an example:

```TypeScript
let upperCaseString : string = "HELLO, TYPESCRIPT!";
let lowerCaseString : string = upperCaseString.toLowerCase();
console.log(lowerCaseString);  // Outputs: "hello, typescript!"
```

## Deep Dive
Historically, transforming a string's case has been a necessary procedure in various programming languages, including TypeScript's precursor, JavaScript. 

Instead of the toLowerCase method, some languages like Python use the `lower()` function. But the concept remains the same across languages: convert all uppercase characters in a string to lowercase.

In TypeScript, the `toLowerCase()` method is not the only method for manipulating string casing. There's also `toUpperCase()`, which converts all string characters to uppercase, useful when needing to standardize inputs to uppercase.

Implementation-wise, the `toLowerCase()` function works character by character, checking if they are within the ASCII range of uppercase letters (`65 - 90` for `A-Z`). If they fall within this range, it adds 32 to their ASCII value to get the corresponding lowercase letter.

## See Also
- [MDN Documentation on toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Stackoverflow Discussion on String Case Conversion](https://stackoverflow.com/questions/2140627/how-to-do-case-insensitive-string-comparison)
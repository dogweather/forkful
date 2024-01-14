---
title:                "TypeScript recipe: Converting a string to lower case"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

In programming, it's common to encounter strings that need to be converted to lower case for various reasons such as data validation or user input. This simple task may seem trivial, but it can save you from potential errors and make your code more user-friendly.

## How To

Converting a string to lower case in TypeScript is quick and easy. You can achieve this using the `.toLowerCase()` method. Let's look at an example:

```TypeScript
let myString = "Hello World";
console.log(myString.toLowerCase());
```

In this code snippet, we first declared a variable `myString` with the value "Hello World". Then, we used the `.toLowerCase()` method to convert the string to lower case and logged the result, which would be "hello world" in this case.

You can also use the `.toLowerCase()` method directly on a string without declaring a variable:

```TypeScript
console.log("Hello World".toLowerCase());
```

This will produce the same result as the previous example. The `.toLowerCase()` method does not mutate the original string and instead returns a new string in lower case.

## Deep Dive

The `.toLowerCase()` method is a built-in method in JavaScript and TypeScript, so you don't need to import any external libraries. It converts all characters in a string to their lower case equivalents according to the Unicode character set. This means that it can handle various languages and special characters.

Keep in mind that the `.toLowerCase()` method does not convert numbers to lower case, as they do not have lower case equivalents. It also does not affect any characters that are already in lower case.

There are other methods that you can use for string manipulation such as `.toUpperCase()` or `.replace()`, but the `.toLowerCase()` method is handy when you specifically want to convert a string to lower case.

## See Also

- [MDN Web Docs: String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScript Documentation: String Methods](https://www.typescriptlang.org/docs/handbook/string-methods.html#tolowercase-and-touppercase)
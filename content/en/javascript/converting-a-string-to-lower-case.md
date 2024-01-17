---
title:                "Converting a string to lower case"
html_title:           "Javascript recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case in Javascript refers to changing all the letters in a string to lowercase. Programmers do this for various reasons, such as standardizing the input for user data or for string comparisons in code.

## How to:

To convert a string to lower case in Javascript, you can use the built-in method `toLowerCase()`. Let's take a look at an example:

```javascript
let str = "Hello, World!";
let lowerStr = str.toLowerCase();
console.log(lowerStr);
```

**Output:** hello, world!

In this code, we first declare a string `str` with the value "Hello, World!". Then, we use the `toLowerCase()` method to convert the string to lower case and assign the result to a new variable `lowerStr`. Finally, we log `lowerStr` to the console, which will output "hello, world!".

You can also apply the `toLowerCase()` method directly to the string without assigning it to a variable:

```javascript
let str = "HELLO";
console.log(str.toLowerCase());
```

**Output:** hello

## Deep Dive

- Historical context: Converting strings to lower case has been a common practice in programming for a long time. Before the introduction of built-in methods, developers had to write their own functions to convert strings to lower case.

- Alternatives: Besides the built-in method `toLowerCase()`, there are other ways to convert a string to lower case in Javascript. Some developers prefer using regular expressions, while others use external libraries.

- Implementation details: The `toLowerCase()` method uses the Unicode character database to determine the proper lowercase version of each character. This means that it can handle not only English letters but also characters from other languages.

## See Also

- [String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase) - an alternative to `toLowerCase()` that takes into account the system's locale.
- [Regular Expressions in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) - a comprehensive guide on using regular expressions to manipulate strings in Javascript.
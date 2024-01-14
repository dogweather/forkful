---
title:                "Javascript recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Text is a fundamental aspect of programming and constantly manipulating and changing it is essential for creating dynamic and functional applications. Being able to search and replace text provides developers with the power to easily make changes in their code, saving time and effort.

## How To

To search and replace text in JavaScript, we can use the `replace()` method and regular expressions. The `replace()` method takes in two parameters, the first being the text to search for and the second being the replacement text.

```Javascript
let text = "I love JavaScript!"
text = text.replace("love", "enjoy") // output: "I enjoy JavaScript!"
```

We can also use regular expressions to search for a pattern rather than a specific string. For example, if we want to replace all numbers in a string with an asterisk, we can use the following code:

```Javascript
let text = "I have 5 apples and 3 oranges"
text = text.replace(/[0-9]/g, "*") // output: "I have * apples and * oranges"
```

We can also use the `replace()` method with a callback function, which gives us more control over the replacement process. The callback function takes in the matched string as a parameter and returns the replacement string.

```Javascript
let text = "I am learning to code"
text = text.replace(/([a-z]+)/g, (match) => match.toUpperCase()) // output: "I AM LEARNING TO CODE"
```

## Deep Dive

Regular expressions, also known as regex, are a powerful tool for searching and manipulating text. They allow us to define patterns and use them to find and replace specific parts of a string. In JavaScript, we can use the `RegExp` object to create regular expressions.

Regular expressions consist of a pattern enclosed in forward slashes, with optional flags for case sensitivity, global searching, and more.

- `g` flag: global search, matches all occurrences of the pattern.
- `i` flag: case-insensitive search, ignores case when matching.
- `m` flag: multi-line search, matches the pattern across multiple lines.

Apart from using regular expressions for searching and replacing, we can also use them for validation and data extraction.

## See Also

- [MDN Web Docs on string replace() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions 101](https://regex101.com/)
- [JavaScript Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/javascript)
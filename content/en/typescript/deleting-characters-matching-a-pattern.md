---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern means removing from a string every character that fits a certain criterion. Programmers do this when they need to sanitize or standardize user-input data, cleanse files, or prepare text for machine learning tasks.

## How to:

```TypeScript
let str: string = 'Hello $World!@ This is42 a test string';
let strippedStr: string = str.replace(/[\d@$!]/g, '');
console.log(strippedStr); // Output: 'Hello World! This is a test string'
```

The code above is a simple example of removing all numbers, dollar signs, 'at' symbols, and exclamation points from the string `'Hello $World!@ This is42 a test string'`. The resulting string is `'Hello World! This is a test string'`. As you can see, removing characters from a string in TypeScript is a piece of cake.

## Deep Dive

Historically, different programming languages have provided various ways to delete characters matching specific patterns. Regex, short for regular expressions, has stood the test of time as a robust and flexible way to match patterns in text across all languages, TypeScript included.

As an alternative to `replace()`, you could use a combination of TypeScript's `split()`, `filter()`, and `join()` functions to achieve the same result. Here's a quick example:

```TypeScript
let str: string = 'Hello $World!@ This is42 a test string';
let regexPattern = /[\d@$!]/g;
let strippedStr: string = str.split('').map((c) => c.match(regexPattern) ? '' : c).join('');
console.log(strippedStr); // Output: 'Hello World! This is a test string'
```

One thing to note is that the `replace()` function used in our first example doesn't modify the original string but instead returns a new one. This follows a functional programming paradigm where data is immutable and side effects are limited.

## See Also

For more information on TypeScript programming, take a trip across the net:

- String replace() method: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Dive into regular expressions: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Functional Programming in JavaScript: [https://www.smashingmagazine.com/2014/07/dont-be-scared-of-functional-programming/](https://www.smashingmagazine.com/2014/07/dont-be-scared-of-functional-programming/)
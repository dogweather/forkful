---
title:                "Deleting characters matching a pattern"
html_title:           "TypeScript recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a certain pattern is a common task for programmers, especially when working with large amounts of text or data. It involves searching for and removing specific characters or groups of characters that follow a certain pattern, such as removing all vowels from a string or removing all numbers from a list of data. This technique allows for manipulating and cleaning up data in a more efficient and precise manner.

## How To:

```TypeScript
// Example 1: Removing all vowels from a string

let str = "Hello World!";
let newStr = str.replace(/[aeiou]/gi, ''); // regex to remove all vowels
console.log(newStr); // Output: Hll Wrld!

// Example 2: Removing all numbers from an array

let numbers = [ 1, 2, 3, "four", 5, "six" ];
let filteredNumbers = numbers.filter(num => typeof num !== "number");
console.log(filteredNumbers); // Output: ["four", "six"]
```

## Deep Dive:

- Historical Context:
The concept of deleting characters matching a certain pattern has been around for a long time, with the introduction of regular expressions in the 1950s. However, with the rise of modern programming languages like TypeScript, it has become much easier and more efficient to perform these tasks.
- Alternatives:
While regular expressions are a powerful tool for deleting characters matching a pattern, there are also other alternatives such as using built-in string methods like "replace" or "filter". These methods may be better suited for simpler tasks and can be used in conjunction with regular expressions for more complex patterns.
- Implementation Details:
In TypeScript, regular expressions are written between forward slashes (/) and followed by optional flags (such as "gi" in the example). The "g" flag stands for global, allowing the search to continue after finding a match, and the "i" flag stands for case-insensitive, meaning it will match both uppercase and lowercase characters.

## See Also:

- [MDN web docs on regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regular Expressions in TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Replacing patterns with TypeScript](https://dominik.honnef.co/posts/2012/09/replace-pattern-in-string-with-typescript/)
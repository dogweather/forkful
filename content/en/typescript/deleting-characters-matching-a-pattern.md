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

## Why

Deleting characters matching a pattern can be an important task in many programming scenarios. It allows developers to efficiently manipulate strings and remove unwanted characters based on a specific criteria, resulting in clean and structured code.

## How To

To delete characters matching a pattern in TypeScript, we can use the native string method `replace()` combined with regular expressions. The `replace()` method takes in two parameters: the pattern to be replaced and the replacement string. We can use the global flag `g` in the regular expression to match all occurrences of the pattern.

```TypeScript
const sentence = "Hello World!";
const newSentence = sentence.replace(/o/g, "");
console.log(newSentence);

// Output: Hll Wrld!
```

In the example above, we use a regular expression to match all lowercase "o" letters in the string and replace them with an empty string, effectively deleting them.

We can also use the `replace()` method with a function as the replacement parameter. This allows us to perform more complex operations on the matched pattern before replacing it.

```TypeScript
const sentence = "I have 3 apples, 2 oranges, and 1 banana.";
const newSentence = sentence.replace(/\d+/g, (match) => parseInt(match) * 2);
console.log(newSentence);

// Output: I have 6 apples, 4 oranges, and 2 banana.
```

In this example, we use a regular expression to match all numbers and multiply them by 2 before replacing them in the string.

## Deep Dive

Regular expressions are a powerful tool for pattern matching and can be used in various programming languages. In TypeScript, regular expressions are declared between two forward slashes (`/`). The `g` flag is used to match all occurrences of the pattern, and the `i` flag can be used for case-insensitive matching.

Some commonly used metacharacters in regular expressions for pattern matching are:

- `.` : Matches any single character.
- `\d` : Matches any digit.
- `\w` : Matches any word character (a-z, A-Z, 0-9, _).
- `\s` : Matches any whitespace character (space, tab, newline, etc.).
- `[]` : Matches any character within the brackets.
- `[^]` : Matches any character not within the brackets.
- `+` : Match one or more instances of the previous character.
- `*` : Match zero or more instances of the previous character.
- `?` : Match zero or one instance of the previous character.

It's important to note that regular expressions can quickly become complex, so it's recommended to test and practice them before using them in production code.

## See Also

- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Official Documentation - Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Regexr - Online Regular Expression Testing Tool](https://regexr.com/)
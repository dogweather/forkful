---
title:                "Deleting characters matching a pattern"
html_title:           "Javascript recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters that match a specific pattern can be a common task in Javascript, especially when handling user input or manipulating strings. It allows for efficient and accurate data processing, making it a valuable skill for any Javascript developer.

## How To

To delete characters matching a pattern, we can use the `replace` method in combination with a regular expression. The `replace` method takes in two parameters: the pattern to match and the replacement value. To delete the matched characters, we can simply pass an empty string as the replacement value. Here is an example:

```Javascript
// Replace all numbers from a string with an empty string
let str = "Hello 123 World";
let newStr = str.replace(/[0-9]/g, "");
console.log(newStr); // Output: Hello World
```

In this example, we used the regular expression `/[0-9]/g` to match all numbers in the string and replaced them with an empty string. The `g` at the end of the regular expression stands for global, which means it will replace all occurrences of the matched characters.

We can also use other regular expression modifiers, such as `i` for case-insensitive matching, `m` for multiline matching, and `s` for dot-all matching.

```Javascript
// Replace all vowels from a string with an empty string, case-insensitive
let str = "Hello World";
let newStr = str.replace(/[aeiou]/gi, "");
console.log(newStr); // Output: Hll Wrld
```

## Deep Dive

Regular expressions in Javascript can become quite complex, allowing for advanced and specific pattern matching. Here are some common metacharacters and their meanings:

- `.` Matches any character.
- `^` Matches the beginning of a string.
- `$` Matches the end of a string.
- `*` Matches zero or more occurrences of the previous character.
- `+` Matches one or more occurrences of the previous character.
- `?` Matches zero or one occurrence of the previous character.
- `\d` Matches any digit.
- `\w` Matches any alphanumeric character.
- `\s` Matches any whitespace character.
- `[...]` Matches any character within the brackets.
- `[^...]` Matches any character that is not within the brackets.

By combining these metacharacters and modifiers, we can create powerful regular expressions to match and delete any desired characters.

## See Also

- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/javascript)
- [Regular Expressions Explained](https://regexone.com/)
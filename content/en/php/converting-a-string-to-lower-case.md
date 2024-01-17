---
title:                "Converting a string to lower case"
html_title:           "PHP recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case means changing all the letters in a string to their lower case equivalents. Programmers do this to ensure consistency in their code and to make it easier to compare strings without worrying about case sensitivity.

## How to:

To convert a string to lower case in PHP, you can use the built-in function `strtolower()`. It takes in one parameter, the string you want to convert, and returns a new string with all lowercase letters. Here's an example:

```PHP
$string = "Hello World!";
echo strtolower($string);
```

Output: `hello world!`

If the string contains any non-alphabetic characters, like numbers or symbols, they will remain unchanged. For instance:

```PHP
$string = "HeLLo 123";
echo strtolower($string);
```

Output: `hello 123`

## Deep Dive:

### Historical Context

The concept of converting strings to lower case may seem trivial now, but in the early days of computing, it was not uncommon for different systems and programming languages to use different letter cases. This led to compatibility issues and confusion, and thus, the need for a standard emerged. Most programming languages now follow the convention of using lower case letters.

### Alternatives

Apart from `strtolower()`, PHP also has two other functions for case conversion: `strtoupper()` and `ucfirst()`. The former converts a string to all upper case, while the latter capitalizes the first letter of a string. Additionally, some programming languages, like JavaScript, have a single `toLowerCase()` method that can convert a string to either lower or upper case depending on the arguments provided.

### Implementation Details

Behind the scenes, the `strtolower()` function uses the ASCII table to map each character to its lower case equivalent. This means non-ASCII characters, like accented letters or emojis, will not be affected and will remain in their original form.

## See Also:

- [PHP Manual for strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [JavaScript toLowerCase() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
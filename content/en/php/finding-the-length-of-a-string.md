---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string computes the number of characters in a given string. Programmers do it to manage data effectively, for manipulation purposes, or to check the validity of input data.

## How to:

In PHP, you use the `strlen()` function to find the length of a string. Here's a simple example:

```PHP
$text = "Hello, world!";
$length = strlen($text);
echo $length;
```
Sample output of this code is `13`.

For multi-byte characters (e.g., emojis), use `mb_strlen()`:

```PHP
$text = "Hello, world! 😃";
$length = mb_strlen($text, 'UTF-8');
echo $length;
```
Sample output now is `14`.

## Deep Dive

1. **Historical Context**: Previously, PHP programmers mainly relied on `strlen()` to find the length of a string. However, with internationalization and growing character diversity in mind, `mb_strlen()` was introduced to correctly handle multi-byte characters.

2. **Alternatives**: Besides `strlen()` and `mb_strlen()`, there are alternatives like `iconv_strlen()` or `grapheme_strlen()`. These functions might yield different results for the same string, depending on the encoding.

3. **Implementation Details**: `strlen()` simply counts the number of bytes in a string. `mb_strlen()`, meanwhile, takes into account that a single character can be represented by more than one byte.

## See Also

- PHP Documentation on `strlen()` [link](https://www.php.net/manual/en/function.strlen.php)
- PHP Documentation on `mb_strlen()` [link](https://www.php.net/manual/en/function.mb-strlen.php)
- Stack Overflow thread on "Why should I use `mb_strlen()` instead of `strlen()`?" [link](https://stackoverflow.com/questions/3656713/why-should-i-use-mb-strlen-instead-of-strlen)
- PHP Manual on character encoding [link](https://www.php.net/manual/en/mbstring.supported-encodings.php)
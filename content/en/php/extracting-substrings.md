---
title:                "Extracting substrings"
html_title:           "PHP recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Simply put, extracting substrings in PHP means taking a small part of a larger string. Programmers use this technique to manipulate and work with specific portions of a string without affecting the rest of it. This helps make their code more efficient and precise.

## How to:
To extract substrings in PHP, we use the `substr()` function. Let's take a look at an example:

```PHP
$string = "Hello World";
$subString = substr($string, 6, 5);
echo $subString;
```
The first argument in the `substr()` function is the original string, and the second argument is the starting index of the substring. The third argument is optional and represents the length of the substring. In this example, we start at index 6, which is the 'W' in "World" and include the next 5 characters, resulting in "World" being printed as the output.

Another example using the optional length parameter:
```PHP
$string = "I love coding in PHP";
$subString = substr($string, 7);
echo $subString;
```
In this example, we start at index 7, which is the space after "love", and include all the rest of the characters in the string. The output will be "coding in PHP".

## Deep Dive:
The `substr()` function was introduced in PHP 4 and has been a handy tool for programmers since then. Before its existence, programmers had to use more complicated and less efficient methods to extract substrings.

There is also an alternative function, `mb_substr()`, which is a multi-byte safe version of `substr()`. This means it can handle strings with non-ASCII characters correctly. In cases where you are working with multibyte characters, it is recommended to use `mb_substr()` instead.

Internally, `substr()` uses the `memcmp()` function to compare bytes of the string. This is done in a relatively efficient way to avoid excessive memory usage for large strings.

## See Also:
- PHP manual for substr(): http://php.net/manual/en/function.substr.php
- PHP manual for mb_substr(): http://php.net/manual/en/function.mb-substr.php
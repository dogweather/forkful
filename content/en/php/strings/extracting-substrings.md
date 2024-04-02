---
date: 2024-01-20 17:46:05.047707-07:00
description: "Extracting substrings means pulling specific portions from a string.\
  \ Programmers do it to process or manipulate data, like fetching a username from\
  \ an\u2026"
lastmod: '2024-03-13T22:45:00.156628-06:00'
model: gpt-4-1106-preview
summary: "Extracting substrings means pulling specific portions from a string. Programmers\
  \ do it to process or manipulate data, like fetching a username from an\u2026"
title: Extracting substrings
weight: 6
---

## What & Why?
Extracting substrings means pulling specific portions from a string. Programmers do it to process or manipulate data, like fetching a username from an email address or a file extension from a filename.

## How to:
PHP offers several functions to extract substrings. Let's check out `substr`, `mb_substr`, and `strstr`.

```PHP
$string = "Hello, World! Programming is fun.";

// Extract 'World' using substr.
echo substr($string, 7, 5); // Output: World

// UTF-8 string example with mb_substr for multi-byte characters.
$utf8String = "こんにちは世界";
echo mb_substr($utf8String, 5, 2); // Output: 世

// Get everything after the comma with strstr.
echo strstr($string, ","); // Output: , World! Programming is fun.
```

## Deep Dive
Back in PHP's early days, the main way to snatch a piece of a string was `substr()`. However, `substr()` had (and still has) a limit: it doesn't play well with non-English characters (like Japanese or Arabic).

Enter `mb_substr()`, the multibyte-safe counterpart that respects characters from various encodings. It ensures that when you yank a substring, you're not tearing through a character mid-byte, which is crucial for international applications.

`strstr()`, on the other hand, finds the first occurrence of a substring and gives you everything after it. There's also `strchr()` which is an alias of `strstr()`.

While `substr()` and `mb_substr()` let you specify precisely where to start and how much to take, `strstr()` is more like a "find and give me the rest" tool.

## See Also
Here's some extra reading if you're hungry for more:

- PHP Official Documentation for string functions: https://www.php.net/manual/en/ref.strings.php
- A deep dive into PHP's multibyte string functions: https://www.php.net/manual/en/book.mbstring.php
- More about character encoding and why it matters: http://kunststube.net/encoding/

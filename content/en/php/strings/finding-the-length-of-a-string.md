---
date: 2024-01-20 17:47:46.456582-07:00
description: "Finding the length of a string means determining how many characters\
  \ it consists of. Programmers often need this information for tasks like validating\u2026"
lastmod: 2024-02-19 22:05:18.621578
model: gpt-4-1106-preview
summary: "Finding the length of a string means determining how many characters it\
  \ consists of. Programmers often need this information for tasks like validating\u2026"
title: Finding the length of a string
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string means determining how many characters it consists of. Programmers often need this information for tasks like validating input, managing substrings, or simply formatting output.

## How to:

Use the `strlen()` function like this:

```php
<?php
$text = "Hello, world!";
$length = strlen($text);
echo $length; // Output: 13
?>
```

If you run this, you'll see `13` on your screen because "Hello, world!" is 13 characters long, including the space and exclamation point.

## Deep Dive

The `strlen()` function has been part of PHP since the early versions. It's straightforward and works based on the number of bytes, which is usually equivalent to the number of characters in strings without special encoding considerations.

However, with the internationalization of web applications, dealing with multiple languages and character encodings became usual. Characters in UTF-8, for instance, can use more than one byte. That's where `mb_strlen()` comes in:

```php
<?php
// A string with multibyte characters
$multibyteText = "こんにちは";
$length = mb_strlen($multibyteText, "UTF-8");
echo $length; // Output: 5
?>
```

Five characters, but more bytes. The `mb_strlen()` function respects character encoding, ensuring accurate length checks for multibyte strings.

`strlen()` is fast and suitable for single-byte character sets. `mb_strlen()`, while slightly slower due to its need to handle more complex encoding, is necessary when working with internationalized text.

## See Also

- [PHP `strlen()` official documentation](https://www.php.net/manual/en/function.strlen.php)
- [PHP `mb_strlen()` official documentation](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP Multibyte String extension](https://www.php.net/manual/en/book.mbstring.php)

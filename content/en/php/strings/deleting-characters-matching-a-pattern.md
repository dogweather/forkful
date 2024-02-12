---
title:                "Deleting characters matching a pattern"
aliases: - /en/php/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:37.561708-07:00
model:                 gpt-4-1106-preview
simple_title:         "Deleting characters matching a pattern"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern in PHP is about finding specific sequences of characters in strings and removing them. Programmers do this to clean data, format output, or manipulate strings to meet specific criteria, such as removing non-alphanumeric characters from user input for security reasons.

## How to:

PHP uses the `preg_replace` function to delete characters that match a pattern using regular expressions. Here's how to strip digits from a string:

```PHP
<?php
$text = "Year 2023!";
$pattern = '/\d+/'; // Pattern to match all digits
$result = preg_replace($pattern, '', $text);
echo $result; // Outputs: Year !
?>
```

And here's how to remove whitespace:

```PHP
<?php
$text = "Too   many      spaces!";
$pattern = '/\s+/'; // Pattern to match all whitespace
$result = preg_replace($pattern, ' ', $text);
echo $result; // Outputs: Too many spaces!
?>
```

## Deep Dive

Deleting characters by matching patterns isn't new. PHP's `preg_replace` function, which powers this functionality, uses Perl-compatible regular expressions, a staple of text processing since Perl's rise in the late '80s. Alternatives to `preg_replace` include `str_replace` for simple replacements and `trim`, `ltrim`, and `rtrim` for removing whitespaces from strings. For more nuanced pattern deletions, `preg_replace_callback` can be used for additional control during the replacement process.

It's useful to know that the PREG in `preg_replace` stands for Perl Regular Expressions, signifying PHP's usage of Perl's pattern syntax. Here's the breakdown:

- `\d` matches any digit. Adding `+` means "one or more" of the preceding element (digits, in this case).
- `\s` finds any whitespace. Like numbers, `+` after `\s` targets long stretches of space.

Choosing between `preg_replace` and its alternatives depends on what you're doing. Use `preg_replace` for complicated patterns and `str_replace` when dealing with simple, direct substitutions. 

Remember, misuse of regular expressions can lead to inefficient code. Always benchmark and use regular expressions smartly.

## See Also

For more on PHP's string functions and pattern matching:
- [PHP Manual — preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP Manual — Regular Expressions (Perl-Compatible)](https://www.php.net/manual/en/book.pcre.php)
- [PHP Manual — str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP Manual — String Functions](https://www.php.net/manual/en/ref.strings.php)

These links lead to the official PHP documentation where you can dive into the details of string manipulation and pattern matching.

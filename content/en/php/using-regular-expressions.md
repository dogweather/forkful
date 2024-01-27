---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are search patterns used to match character combinations in strings. Programmers use them for tasks like validation, searching, and parsing text, because they're powerful and save time.

## How to:
To use regex in PHP, you typically use `preg_match` for finding a match, or `preg_replace` for search-and-replace. Here's a quick look:

```php
<?php
$string = "The quick brown fox jumps over the lazy dog.";

// Check if 'quick' is in the string
if (preg_match("/quick/", $string)) {
  echo "Match found!";
} else {
  echo "No match found.";
}
// Output: Match found!

// Replace 'brown' with 'red'
$replacedString = preg_replace("/brown/", "red", $string);
echo $replacedString;
// Output: The quick red fox jumps over the lazy dog.
?>
```

## Deep Dive
Regular expressions have been around since the 1950s and were implemented in Perl extensively, influencing many other languages, including PHP. Alternatives to regex in PHP include functions like `strpos()` for finding substrings or `str_replace()` for replacing text. The PCRE (Perl Compatible Regular Expressions) library is what PHP uses under the hood for regex functions, offering rich and powerful pattern-matching capabilities.

## See Also
- [PHP Official Documentation on PCRE](https://www.php.net/manual/en/book.pcre.php)
- [Regular-Expressions.info](https://www.regular-expressions.info/) - for a thorough understanding of regex.
- [Regex101](https://regex101.com/) - for testing and debugging your regex patterns.

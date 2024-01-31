---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means making the first letter of each word uppercase. Programmers capitalize strings for visual consistency, branding, or user experience design.

## How to:
In PHP, we capitalize strings with the `ucwords()` function for full titles or `ucfirst()` for single lines or sentences.

```php
<?php
$lowercase_title = "the quick brown fox jumps over the lazy dog";
$capitalized_title = ucwords($lowercase_title);

echo $capitalized_title; // Outputs: The Quick Brown Fox Jumps Over The Lazy Dog

$sentence = "an example sentence.";
$capitalized_sentence = ucfirst($sentence);

echo $capitalized_sentence; // Outputs: An example sentence.
?>
```

## Deep Dive
Capitalizing strings is not a new concept. In the print world, title capitalization is a standard convention. In PHP, `ucwords` and `ucfirst` have been around for a while, empowering such conventions digitally. PHP's `mb_convert_case` function allows more complex manipulations, like `MB_CASE_TITLE`, especially useful for multibyte (non-ASCII) strings.

Alternatives to `ucwords` include `strtoupper`, which converts the whole string to uppercase, and `strtolower` which makes the string lowercase. Be mindful of locale: certain languages have unique capitalization rules.

Implementation wise, `ucwords` applies uppercase to the first character after any whitespace, not just spaces. This means new lines, tabs, etc., all trigger capitalization.

## See Also
For more information check out:

- PHP Manual on `ucwords()`: https://www.php.net/manual/en/function.ucwords.php
- PHP Manual on `ucfirst()`: https://www.php.net/manual/en/function.ucfirst.php
- PHP Manual on `mb_convert_case()`: https://www.php.net/manual/en/function.mb-convert-case.php
- PHP string functions: https://www.php.net/manual/en/ref.strings.php

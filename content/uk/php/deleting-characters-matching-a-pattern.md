---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:42:48.171162-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Deleting characters matching a pattern means we’re getting rid of specific letters or symbols based on rules—like tossing out all vowels from a sentence. Programmers do this to clean data, shape inputs for databases, or simplify text for processing.

## How to (Як це зробити):
Let's remove all digits from a string using PHP's preg_replace function:

```php
<?php
$text = "Order123 has been processed on 2021-09-15.";
$pattern = '/[0-9]+/';
$replacement = '';

$cleanedText = preg_replace($pattern, $replacement, $text);

echo $cleanedText; // Outputs: Order has been processed on -.
?>
```

## Deep Dive (Поглиблено):
Deleting characters by pattern has been integral since Perl introduced powerful regular expressions (regex) in the 1980s. PHP adopted regex, allowing such operations with preg_replace. Alternatives include str_replace (for specific strings) and substr_replace. For performance, regex could be slower than these, but offers more flexibility. It’s implemented via the PCRE (Perl Compatible Regular Expressions) library, which ensures compatibility with Perl’s regex patterns.

## See Also (Додатково):
- PHP Official Documentation on `preg_replace`: https://www.php.net/manual/en/function.preg-replace.php
- Regex101 for testing patterns: https://regex101.com/
- PCRE Manual: https://www.pcre.org/original/doc/html/

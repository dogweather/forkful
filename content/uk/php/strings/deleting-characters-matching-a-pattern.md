---
date: 2024-01-20 17:42:48.171162-07:00
description: "Deleting characters matching a pattern means we\u2019re getting rid\
  \ of specific letters or symbols based on rules\u2014like tossing out all vowels\
  \ from a sentence.\u2026"
lastmod: '2024-03-13T22:44:49.404037-06:00'
model: gpt-4-1106-preview
summary: "Deleting characters matching a pattern means we\u2019re getting rid of specific\
  \ letters or symbols based on rules\u2014like tossing out all vowels from a sentence."
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

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

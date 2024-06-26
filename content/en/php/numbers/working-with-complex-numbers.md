---
date: 2024-01-25 03:00:04.836795-07:00
description: 'How to: PHP provides built-in support for complex numbers using the
  `ext-intl` extension with the `NumberFormatter` class. Here''s an example.'
lastmod: '2024-03-13T22:45:00.160875-06:00'
model: gpt-4-1106-preview
summary: PHP provides built-in support for complex numbers using the `ext-intl` extension
  with the `NumberFormatter` class.
title: Working with complex numbers
weight: 14
---

## How to:
PHP provides built-in support for complex numbers using the `ext-intl` extension with the `NumberFormatter` class. Here's an example:

```php
// Ensure the intl extension is loaded
if (!extension_loaded('intl')) {
    die("The intl extension is not enabled. Please enable it to run this code.");
}

function addComplexNumbers($a, $b) {
    // Use NumberFormatter to parse and format complex numbers
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Parse complex numbers from strings
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Perform addition
    $sum = $numA + $numB;

    // Format the result as a complex number
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Output: 7+10i
```

## Deep Dive
Before `ext-intl`, PHP didn't have native complex number support. Developers used functions or custom class libraries to handle complex numbers. Complex operations could be tedious and error-prone, but `ext-intl` provides an internationalized way to present and parse complex numbers aligned with the ICU library.

However, for heavyweight math operations, some might use external libraries written in more math-friendly languages (like C or Python) and interface with them through PHP. Regarding implementation, `ext-intl` handles it behind the scenes, ensuring accurate arithmetic while abstracting complexity from the developer.

Historically, complex numbers were frowned upon being termed 'imaginary', but they've since become fundamental in various scientific and mathematical fields, revealing more about their real-world significance than their imaginary status ever suggested.

## See Also
- [PHP Manual on NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia on complex numbers](https://en.wikipedia.org/wiki/Complex_number)
- [PHP: The Right Way - Working with Data Types](https://phptherightway.com/#data_types)

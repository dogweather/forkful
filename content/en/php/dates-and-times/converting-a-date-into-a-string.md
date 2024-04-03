---
date: 2024-01-20 17:36:54.554313-07:00
description: "How to: In PHP, the `date()` function formats a timestamp to a more\
  \ readable string. The `DateTime` object serves a similar purpose with its `format()`\u2026"
lastmod: '2024-03-13T22:45:00.177384-06:00'
model: gpt-4-1106-preview
summary: In PHP, the `date()` function formats a timestamp to a more readable string.
title: Converting a date into a string
weight: 28
---

## How to:
In PHP, the `date()` function formats a timestamp to a more readable string. The `DateTime` object serves a similar purpose with its `format()` method. Here's how they look in practice:

```php
<?php
// Using date() function
echo date('Y-m-d H:i:s') . "\n"; // output: 2023-04-03 14:30:00 (example)

// Using DateTime object
$dateTime = new DateTime();
echo $dateTime->format('Y-m-d H:i:s') . "\n"; // output: identical
?>
```
Sample output reflects the date and time the code was run.

## Deep Dive
Historically, PHP has evolved in handling date and time. Early PHP versions had fewer date manipulation features. The `DateTime` class, introduced in PHP 5.2.0, provided object-oriented handling, timezone support, and more versatility.

Alternatives to `date()` and `DateTime` include:
- `strftime()` (locale-aware formatting)
- `DateTimeImmutable` (immutable version of `DateTime`)
- Extension classes like `Carbon` for more complex needs

Internally, both `date()` and `DateTime` rely on the server's timezone settings unless otherwise specified. The `DateTimeZone` class can manipulate timezones.

## See Also
- [PHP Manual: Date and Time Functions](https://www.php.net/manual/en/book.datetime.php)
- [PHP The Right Way: Dates and Times](https://phptherightway.com/#date_and_time)
- [Carbon: A simple PHP API extension for DateTime](https://carbon.nesbot.com/)

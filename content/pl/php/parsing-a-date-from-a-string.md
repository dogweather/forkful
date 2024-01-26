---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:37:32.661143-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Parsing a date means converting a date written as text into a format that PHP understands and can operate on. We do it because we often get dates in a human-readable form (like from a form input), but need them in a machine-friendly format to store, calculate, or manipulate.

## How to: (Jak to zrobić:)
```PHP
<?php
$dateString = "2023-03-15 14:00:00";
$parsedDate = date_create_from_format("Y-m-d H:i:s", $dateString);

echo $parsedDate->format("Y-m-d H:i:s"); // Wyświetla: 2023-03-15 14:00:00
?>
```

## Deep Dive (Dogłębna analiza)
PHP used `strtotime()` and `DateTime` objects to parse date strings. `DateTime` is more versatile, allowing OOP (Object-Oriented Programming) approaches. Historically, PHP's date parsing has improved to better handle different formats and timezones. Alternatives include using `DateTimeImmutable` for immutable objects or procedural `date_parse_from_format()`. DateTime objects store internal date data, leveraging PHP's built-in functions to handle complexities of dates and times.

## See Also (Zobacz również)
- PHP Manual on DateTime: https://www.php.net/manual/en/class.datetime.php
- strtotime() function reference: https://www.php.net/manual/en/function.strtotime.php
- date_create_from_format() function reference: https://www.php.net/manual/en/function.date-create-from-format.php

---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:37:40.081063-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means converting text that represents a date and time into a programmable format. Programmers do this to manipulate dates, compare them, or store them in a database efficiently.

## How to:

PHP makes parsing dates from strings fairly straightforward with the `DateTime` class. Here's a quick example:

```php
<?php
$dateString = '2023-04-12 14:00:00';
$dateTime = new DateTime($dateString);

echo $dateTime->format('Y-m-d H:i:s'); // Outputs: 2023-04-12 14:00:00
?>
```

Simple, right? Now, want to change the format or timezone? Here's how:

```php
<?php
$dateString = 'April 12, 2023 14:00:00';
$dateTime = DateTime::createFromFormat('F j, Y H:i:s', $dateString);
$dateTime->setTimezone(new DateTimeZone('Europe/London'));

echo $dateTime->format('Y-m-d H:i'); // Outputs: 2023-04-12 14:00
?>
```

Play with formats and timezones to see how powerful this can be.

## Deep Dive

Historically, PHP developers had to manually parse date strings or use `strtotime()` which is effective but less powerful than `DateTime`. Introduced in PHP 5.2.0, `DateTime` provides object-oriented date/time manipulation.

Why the change? Because `DateTime`:

1. Handles exceptions.
2. Works with different calendars.
3. Is timezone-aware.
4. Has more formatting and parsing options. 

Alternatives include the `IntlDateFormatter` for internationalization or the `Carbon` library for modern syntactic sugar.

When parsing, mind the pitfalls:

- Always validate input. Incorrect formats cause wrong dates or errors.
- Timezones matter. Store in UTC and display locally.
- Leap seconds and daylight saving times can affect computations.

## See Also

- [PHP Manual on DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP Date and Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [Carbon: A simple PHP API extension for DateTime](https://carbon.nesbot.com/)

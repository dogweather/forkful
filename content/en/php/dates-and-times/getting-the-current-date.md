---
date: 2024-02-03 19:02:39.616171-07:00
description: "Getting the current date in PHP is a fundamental task that allows you\
  \ to retrieve and manipulate the system's date and time. This is crucial for functions\u2026"
lastmod: '2024-03-13T22:45:00.176473-06:00'
model: gpt-4-0125-preview
summary: "Getting the current date in PHP is a fundamental task that allows you to\
  \ retrieve and manipulate the system's date and time. This is crucial for functions\u2026"
title: Getting the current date
weight: 29
---

## What & Why?
Getting the current date in PHP is a fundamental task that allows you to retrieve and manipulate the system's date and time. This is crucial for functions such as logging, time-stamping posts, scheduling events, or performing time-sensitive operations in your applications.

## How to:
### Native PHP
PHP's built-in `date()` function is the most direct way to get the current date. You can format the date in various ways by specifying the format parameter.

```php
echo date("Y-m-d"); // Outputs: 2023-04-01 (for example)
echo date("l, F j, Y"); // Outputs: Saturday, April 1, 2023
```

To get the date and time with timezone support, you can use the `DateTime` class along with `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Outputs: 2023-04-01 12:00:00 (for example)
```

### Using Carbon (A Popular Third-Party Library)
[Carbon](https://carbon.nesbot.com/) is a simple API extension for `DateTime` that provides a cleaner and more fluent way to work with dates and times.

First, ensure you have Carbon installed via Composer:
```bash
composer require nesbot/carbon
```

Then, you can use it to get the current date:

```php
use Carbon\Carbon;

echo Carbon::now(); // Outputs: 2023-04-01 12:00:00 (for example, in the default format)
echo Carbon::now()->toDateString(); // Outputs: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Outputs: Saturday, April 1, 2023
```

Carbon enriches the date-time handling in PHP by adding readability and a bounty of functionality for time manipulation, comparison, and formatting.

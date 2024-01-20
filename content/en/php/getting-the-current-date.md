---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Getting the Current Date in PHP: A Quick Guide

## What & Why?
Getting the current date in PHP refers to fetching the instant data about the day, month and year. It's handy for logging events, stamping transactions or simply providing user-friendly, time-aware interfaces.

## How To:
PHP makes it easy. The `date()` function is your friend. Here's an example:

```PHP
<?php
echo "Today is: ". date('Y-m-d');
?>
```
When you run this, the output will be (example):
```
Today is: 2022-06-10
```
You can also format as you wish. Check the [PHP manual](https://www.php.net/manual/en/function.date.php) for formatting options.

## Deep Dive
PHP's `date()` function has been around since PHP 4, adapting over time to keep up with improvements in the language. The function uses the server's default timezone unless specified otherwise. This may be relevant if your application will be used globally.

There are alternatives! You can use `DateTime()` class for more complex operations involving date and time:

```PHP
<?php
$dt = new DateTime();
echo "Today is: ". $dt->format('Y-m-d');
?>
```
This will deliver the same result, with `DateTime()` offering more flexibility if you need to adjust the time zone or perform date arithmetic.

## See Also
For more deep-dives on date and time handling in PHP:

1. [PHP: DateTime - Manual](https://www.php.net/manual/en/class.datetime.php)
2. [PHP Date/Time Functions - W3Schools](https://www.w3schools.com/php/php_ref_date.asp)

Keep coding, keep exploring!
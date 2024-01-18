---
title:                "Parsing a date from a string"
html_title:           "PHP recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string in PHP refers to the process of converting a date input that is in text format into a more usable date object. This is important for programmers because it allows for easier manipulation and comparison of dates, which are commonly used in databases and web applications.

## How to:

To parse a date from a string in PHP, we will use the built-in `strtotime()` function. This function takes in a string representing a date and time and converts it into a Unix timestamp, which is a numerical value representing the number of seconds since January 1, 1970.

```PHP
// Example date string
$date_string = "January 1, 2020";

// Using strtotime to convert to a Unix timestamp
$timestamp = strtotime($date_string);

// Output will be: 1577836800
echo $timestamp;
```

To convert the Unix timestamp back into a readable date format, we can use the `date()` function. This function takes in a format parameter and the Unix timestamp and returns a formatted string representing the date and time.

```PHP
// Using date to format the timestamp into a readable date
// Output will be: January 1, 2020
echo date("F j, Y", $timestamp);
```

## Deep Dive:

Parsing dates from strings has become easier in recent years with the addition of built-in functions like `strtotime()` and `date()` in PHP. In the past, developers had to manually manipulate and convert date strings into usable formats, which could be time-consuming and error-prone.

An alternative to using `strtotime()` and `date()` is the `DateTime` class, which offers more advanced functionality for manipulating and formatting dates. This class also has the ability to handle time zones and daylight saving time, making it a more robust option for handling dates in a variety of situations.

When parsing dates from strings, it is important to understand the format of the input date string to ensure accurate conversion. PHP's date formats follow the same syntax as the Unix `date` command, allowing for consistency in date formatting across different systems.

## See Also:

- [PHP Manual - strtotime()](https://www.php.net/strtotime)
- [PHP Manual - date()](https://www.php.net/date)
- [PHP Manual - DateTime class](https://www.php.net/manual/en/class.datetime.php)
---
title:                "Getting the current date"
html_title:           "PHP recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Getting the current date is a common operation in programming, especially when dealing with time-sensitive tasks. It allows for accurate tracking of events, scheduling tasks, and displaying up-to-date information to users.

## How To

To get the current date in PHP, we can use the built-in function `date()`. This function takes in a format parameter and returns a string representing the current date and time in the specified format.

Let's take a look at some examples:

```
<?php
echo date('d/m/Y'); // Output: 26/11/2021
echo date('l, F jS Y'); // Output: Friday, November 26th 2021
echo date('h:i A'); // Output: 10:30 AM
```

As you can see, we can customize the format of the date to our liking, using a combination of letters and special characters. Here are some commonly used format letters:

- `d` - day of the month (01 to 31)
- `m` - month (01 to 12)
- `Y` - 4-digit year
- `l` - full day of the week (eg: Friday)
- `F` - full month name (eg: November)
- `jS` - day of the month with suffix (eg: 26th)
- `h` - hour in 12-hour format (01 to 12)
- `i` - minutes (00 to 59)
- `A` - AM/PM

You can find the full list of format letters in the [official PHP documentation](https://www.php.net/manual/en/function.date.php).

If you want to get the current date's timestamp, you can use the `time()` function. This function returns the current time in seconds since the Unix Epoch (January 1 1970 00:00:00 GMT).

```
<?php
echo time(); // Output: 1637963400 (will vary based on current time)
```

## Deep Dive

Internally, the `date()` function uses the Unix Timestamp to retrieve the current date and time. It first obtains the current timestamp using the `time()` function, then formats it based on the provided format.

It's worth noting that the `date()` function doesn't change the default time zone of your server. By default, it uses the time zone set in the `date.timezone` configuration directive in your php.ini file. You can check and change this setting using the `date_default_timezone_get()` and `date_default_timezone_set()` functions, respectively.

If you want to get the current date and time in a specific time zone, you can use the `DateTime` class. This class provides an object-oriented interface for working with dates and times and allows for easy time zone conversions.

```
<?php
$timezone = new DateTimeZone('Europe/London');
$date = new DateTime('now', $timezone);
echo $date->format('d/m/Y h:i A'); // Output: 26/11/2021 03:30 PM
```

## See Also

- [PHP date() Function](https://www.php.net/manual/en/function.date.php)
- [PHP time() Function](https://www.php.net/manual/en/function.time.php)
- [PHP DateTime Class](https://www.php.net/manual/en/class.datetime.php)
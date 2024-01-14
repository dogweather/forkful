---
title:                "PHP recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to display the current date on your website or application? Maybe you want to customize the date format or use it for time-sensitive features. Whatever the reason may be, obtaining the current date is often a necessary task in PHP programming.

## How To

There are a few different ways to get the current date in PHP, depending on your specific needs. One method is by using the built-in `date()` function. This function allows you to format the date in various ways, such as showing the day, month, and year, or even adding a specific timezone.

```
<?php
$currentDate = date("F j, Y"); // Outputs date in a month day, year format
echo $currentDate; // Output: May 5, 2021
```

You can also use the `strtotime()` function to convert a string date into a timestamp, which can then be formatted using the `date()` function. This allows for more flexibility in customizing the date format.

```
<?php
$currentTimestamp = strtotime("05/05/2021"); // Converts string date into timestamp
$formattedDate = date("l, F jS Y", $currentTimestamp); // Outputs date in a weekday, month day suffix, year format
echo $formattedDate; // Output: Wednesday, May 5th 2021
```

Another method is by using the `DateTime` class, which offers more functionality for manipulating dates and times. This class provides methods such as `setTimestamp()` to set a specific timestamp and `format()` to customize the date format.

```
<?php
$dateTime = new DateTime();
$dateTime->setTimestamp(1620214800); // Sets timestamp to May 5th, 2021
$date = $dateTime->format("l, F jS Y"); // Outputs date in a weekday, month day suffix, year format
echo $date; // Output: Wednesday, May 5th 2021
```

## Deep Dive

Behind the scenes, PHP uses the Unix timestamp to handle dates and times. This timestamp represents the number of seconds that have elapsed since January 1, 1970, 00:00:00 UTC. This means that when you call functions like `date()` with a specific date format, PHP is converting the current timestamp into a readable date.

There are also various PHP libraries and frameworks that offer more advanced date and time manipulation features. For example, Laravel has a `Carbon` class that extends the `DateTime` class and provides additional methods for manipulating dates and times.

It is worth noting that the current date and time obtained through these methods may vary depending on the server's timezone. To ensure consistency, you can use the `date_default_timezone_set()` function to set a specific timezone for your application.

## See Also

- [PHP Official Documentation on date() function](https://www.php.net/manual/en/function.date.php)
- [PHP Official Documentation on strtotime() function](https://www.php.net/manual/en/function.strtotime.php)
- [PHP Official Documentation on DateTime class](https://www.php.net/manual/en/datetime.php)
- [Laravel Documentation on Carbon class](https://laravel.com/docs/8.x/eloquent-mutators#date-casting)
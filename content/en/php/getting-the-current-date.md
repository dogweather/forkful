---
title:    "PHP recipe: Getting the current date"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

In any programming language, getting the current date is an important task that can have various use cases. In PHP, it can be particularly useful for tasks such as displaying time-sensitive information, logging, or scheduling automated processes. Luckily, PHP has built-in functions to make retrieving the current date a breeze.

## How To

To get the current date in PHP, we can use the built-in `date()` function. This function takes in two parameters: a format and an optional timestamp. The `format` parameter specifies the desired format in which we want to display the date, and the `timestamp` parameter allows us to specify a specific point in time.

Let's look at an example:

```PHP
<?php
// Get the current date in the format of "Month Day, Year"
echo date("F j, Y");
// Output: April 23, 2021

// Get the current date and time in the format of "Timezone"
echo date("e");
// Output: America/New_York
?>
```

In the first example, we used the `F` format for the month, `j` for the day, and `Y` for the 4-digit year. Similarly, we can use a combination of different format parameters to get the desired output. It's worth noting that the `timestamp` parameter defaults to the current time if not specified.

For a full list of available date formats, check out the [PHP documentation](https://www.php.net/manual/en/function.date.php).

## Deep Dive

Under the hood, the `date()` function uses the Unix timestamp to determine the current date and time. Unix timestamp is the number of seconds that have elapsed since January 1, 1970, at 00:00:00 UTC. This timestamp is used to represent date and time in a universal format, regardless of the time zone.

When using the `date()` function, keep in mind that the output will depend on the default timezone set in your PHP configuration or the `date_default_timezone_set()` function. You can change the default timezone or specify a different one by using the `e` or `O` format parameters, as shown in the above example.

## See Also

- [PHP date and time functions](https://www.php.net/manual/en/ref.datetime.php)
- [PHP date formats](https://www.php.net/manual/en/datetime.format.php)
- [Unix timestamp explained](https://en.wikipedia.org/wiki/Unix_time)
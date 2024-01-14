---
title:    "PHP recipe: Getting the current date"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

In today's fast-paced world, keeping track of time is essential for any programmer. Whether it's for scheduling tasks, comparing data, or simply displaying the current date on a website, having access to the current date is crucial. In this blog post, we will explore how to get the current date in PHP and the different ways it can be used in your programming projects.

## How To

To get the current date in PHP, we can use the built-in `date()` function. This function takes two parameters - the format of the date and an optional timestamp. Let's take a look at some examples:

```PHP
// Prints the current date in the default format
echo date('Y-m-d'); // Output: 2021-01-01

// Sets the date format to month/day/year
echo date('m/d/Y'); // Output: 01/01/2021

// Using a timestamp to get a specific date and time
echo date('Y-m-d H:i:s', 1613543700); // Output: 2021-02-17 14:35:00
```

As seen in the examples, we can specify the format of the date using different characters, such as `Y` for year, `m` for month, `d` for day, and `H` for 24-hour time. The full list of format characters can be found in the PHP documentation.

Apart from the `date()` function, we can also use the `DateTime` class to get the current date and manipulate it. Let's see an example:

```PHP
// Creating a new DateTime object with the current date and time
$date = new DateTime();

// Manipulating the date object to a specific format
echo $date->format('l, M d, Y'); // Output: Friday, Jan 01, 2021
```

## Deep Dive

Behind the scenes, PHP gets the current date from the server it is running on. This means that if your server's time is incorrect, the date returned by PHP will also be incorrect. However, we can use the `date_default_timezone_set()` function to set the timezone we want to use. For example:

```PHP
date_default_timezone_set('America/New_York');

echo date('Y-m-d H:i:s'); // Output: 2021-01-01 09:00:00 (assuming server time is set to UTC)
```

Additionally, we can also use the `strtotime()` function to convert a date string into a Unix timestamp. This can be useful when comparing dates or performing calculations. Let's look at an example:

```PHP
$today = strtotime('today'); // Gets the timestamp for the start of the current day
$tomorrow = strtotime('tomorrow'); // Gets the timestamp for the start of the next day
$diff = ($tomorrow - $today) / 60 / 60; // Calculates the difference in hours between today and tomorrow

echo "There are $diff hours between today and tomorrow."; // Output: There are 24 hours between today and tomorrow.
```

## See Also

- [PHP Date Function](https://www.php.net/manual/en/function.date.php)
- [PHP DateTime Class](https://www.php.net/manual/en/class.datetime.php)
- [PHP Timezones](https://www.php.net/manual/en/timezones.php)
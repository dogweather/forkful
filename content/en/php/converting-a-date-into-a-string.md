---
title:                "Converting a date into a string"
html_title:           "PHP recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a fundamental skill for any PHP programmer. It allows you to display dates in a user-friendly format and also gives you more control over how the date is displayed.

## How To

Converting a date into a string is a simple process in PHP. First, we need to create a date object using the `strtotime()` function. This function takes a string representing a date and converts it into a unix timestamp. 

```PHP
$date = strtotime('2020-11-30');
```

Next, we can use the `date()` function to format the date in any way we want. This function takes two parameters - the format we want and the date object we created above.

```PHP
echo date('F jS, Y', $date); // Output: November 30th, 2020
```

The format parameter allows us to customize the output of the date. For example, `F` represents the full month name, `j` represents the day of the month without leading zeros, and `Y` represents the full year. You can find a full list of formatting options in the official PHP documentation.

## Deep Dive

When converting a date into a string, there are a few important things to keep in mind. First, the `strtotime()` function uses the current time zone set in your PHP configuration. If your date is in a different time zone, you can use the `date_default_timezone_set()` function to change it before converting it into a date object.

Another thing to consider is leap years. PHP's `strtotime()` function automatically takes care of leap years, so you don't need to worry about it when formatting your dates.

Additionally, PHP has a `DateTime` class that allows more flexibility and control over formatting dates. This class has many useful methods, such as `format()` and `modify()`, which can be helpful in certain situations.

## See Also

- [PHP date() function documentation](https://www.php.net/manual/en/function.date.php)
- [PHP DateTime class documentation](https://www.php.net/manual/en/class.datetime.php)
- [Time zones in PHP](https://www.php.net/manual/en/datetime.settimezone.php)
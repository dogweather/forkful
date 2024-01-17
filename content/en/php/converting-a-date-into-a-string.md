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

## What & Why?

Date conversion in programming refers to the process of converting a date data type into a string data type. This is commonly done for data manipulation and display purposes, such as when printing a date in a specific format. Programmers often do this to make dates more readable and understandable for both the software and the end user.

## How to:

To convert a date into a string in PHP, you can use the `date()` function. This function takes in two parameters: the format of the date and the date to be converted. Here is an example of converting the current date into a string in the format of "Month, Day Year":

```PHP
$currentDate = date("F j, Y");
echo $currentDate;
```

This will output something like "June 1, 2021." You can also specify a specific date by using the `strtotime()` function, which can convert textual date descriptions into Unix timestamp (the number of seconds since January 1, 1970). For example:

```PHP
$specificDate = date("F j, Y", strtotime("next Monday"));
echo $specificDate;
```

This will output the date of the next Monday in the format of "Month, Day Year." There are many different formats that you can use with the `date()` function, and you can find a full list of them in the "See Also" section below.

## Deep Dive:

Historically, date manipulation has been a common challenge for programmers. The `date()` function was introduced in PHP 4 as a solution to this problem. Prior to this, PHP used the Unix timestamp format to represent dates, which was not very user-friendly. This function has since become a standard method for converting dates into strings in PHP.

An alternative method to converting dates is to use the `strftime()` function, which is similar to the `date()` function but allows for localization for different languages and regions. However, this function is only available on operating systems that support the C strftime() function.

When using the `date()` function, it's important to note that the date and time displayed will be based on the default time zone set in the php.ini file. You can also set a specific time zone using the `date_default_timezone_set()` function.

## See Also:

- PHP documentation for `date()` function: https://www.php.net/manual/en/function.date.php
- PHP documentation for `strtotime()` function: https://www.php.net/manual/en/function.strtotime.php
- PHP documentation for `strftime()` function: https://www.php.net/manual/en/function.strftime.php
---
title:                "PHP recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a common task in PHP programming. It allows you to easily display dates in a readable format and manipulate them for various purposes. As a programmer, understanding how to convert dates into strings is an essential skill to have.

## How To

Converting a date into a string can be done using the `date()` function in PHP. This function takes two parameters - the format of the date and the date to be formatted. Let's take a look at an example:

```
<?php
$date = "2020-08-25";
echo date("F j, Y", strtotime($date));
```

In the code above, we have a date stored in a variable called `$date`. We then used the `date()` function to convert the date to a string in the format of "Month Day, Year". The `strtotime()` function is used to convert the date string into a Unix timestamp, which is required by the `date()` function.

The output of the above code would be:

```
August 25, 2020
```

You can also use different formats to display the date, depending on your needs. For example, you can use "m/d/Y" to display the date in the format of "08/25/2020".

```
<?php
$date = "2020-08-25";
echo date("m/d/Y", strtotime($date));
```

The output of this code would be:

```
08/25/2020
```

## Deep Dive

The `date()` function allows you to use a variety of format characters to customize the output of the date. For example, you can use "l" to display the full day of the week, "M" to display the abbreviated month name, and "Y" to display the full year.

You can also use these format characters to manipulate the date. For example, if you add or subtract a number after the format character, it will change the date accordingly. Let's take a look at an example:

```
<?php
$date = "2020-08-25";
echo date("M j, Y", strtotime($date . ' +1 day'));
```

In the above code, we added the number "1" after the format character "j", which represents the day of the month. This will add one day to the date and display it in the format of "Month Day, Year". The output of this code would be:

```
Aug 26, 2020
```

You can use this technique to perform various date calculations and conversions, making your programming tasks much easier.

## See Also

- Official PHP Documentation for [date() function](https://www.php.net/manual/en/function.date.php)
- [PHP Date Formats Cheat Sheet](https://www.w3schools.com/php/func_date_date_format.asp)
- [Unix Timestamp Converter](https://www.unixtimestamp.com)
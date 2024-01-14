---
title:    "PHP recipe: Calculating a date in the future or past"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# Why

As a programmer, you may often need to calculate dates in the past or future for various purposes such as scheduling tasks, setting reminders, or tracking events. This can be done easily using PHP programming language, which provides a range of built-in functions and methods for date and time manipulation.

# How To

To calculate a date in the future or past, you will need to use PHP's `DateTime` class and its accompanying methods. Here's a simple example of calculating a date 7 days from now:

```
<?php
$currentDate = new DateTime(); //create a new DateTime object
$currentDate->modify("+7 days"); //modify the object by adding 7 days
$newDate = $currentDate->format('Y-m-d'); //format the new date in a desired format
echo "The date 7 days from now is: $newDate"; //output the result
?>
```

The output of this code will be "The date 7 days from now is: 2021-06-10".

You can also calculate dates in the past by using negative values in the `modify()` method. For example, to get the date 3 months ago, you can use `$currentDate->modify("-3 months")`. It is important to note that the `modify()` method modifies the object itself, so make sure to assign it to a new variable if you want to keep the original date object.

# Deep Dive

The `DateTime` class also has many other useful methods for date and time manipulation, such as `add()` and `sub()` for adding or subtracting time intervals, `diff()` for getting the difference between two dates, and `setTimezone()` for converting dates between time zones.

In addition, PHP also has the `strtotime()` function, which can convert a date string into a timestamp, making it easier to perform calculations on dates. It also has the `date()` function for formatting dates according to a specified pattern.

It is important to understand the different date and time formats in PHP to avoid any unexpected results. PHP uses the Unix timestamp format, which represents the number of seconds since January 1, 1970. However, it also has functions to convert dates between different formats.

# See Also
- Official PHP documentation on DateTime class: https://www.php.net/manual/en/class.datetime.php
- Tutorial on manipulating dates and times in PHP: https://www.php.net/date
- Useful date and time functions in PHP: https://www.php.net/manual/en/ref.datetime.php
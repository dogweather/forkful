---
title:                "PHP recipe: Calculating a date in the future or past"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to calculate a date in the future or past while coding in PHP? Maybe you are building a scheduling application or need to display future event dates. Whatever the reason may be, learning how to effectively calculate dates in PHP can save you time and make your code more efficient.

## How To
Calculating dates in PHP is actually quite simple, thanks to the built-in Date and Time functions. To calculate a date in the past or future, we first need to create a Date object using the `date_create()` function. We can then use the `date_modify()` function to add or subtract any amount of time from the original date. Let's take a look at an example:

```
<?php
$date = date_create("2021-01-01");
date_modify($date, "+7 days");
echo date_format($date, "Y-m-d");
```

This code will output `2021-01-08`, which is 7 days after the original date of January 1st, 2021. As you can see, we used the `date_modify()` function to add 7 days to the original date. We can also use this function to subtract time, by using a `-` before the amount (e.g. `"-2 weeks"`).

We can also use the `strtotime()` function to modify dates. This can be useful if we want to add or subtract more specific increments, such as 2 weeks and 3 days. Let's take a look at an example:

```
<?php
$date = date_create("2021-01-01");
date_modify($date, "2 weeks 3 days");
echo date_format($date, "Y-m-d");
```

This code will output `2021-01-17`, which is 2 weeks and 3 days after the original date. As you can see, we used the `date_modify()` function with a specific increment to modify the date.

## Deep Dive
While these examples give a basic understanding of how to calculate dates in PHP, there are many more functions and methods that can be used for more complex calculations. For example, the `DateTime` class has a `diff()` method that allows you to find the difference between two dates, as well as the `add()` and `sub()` methods for adding or subtracting time. It's important to also consider the use of timezones and daylight saving time when calculating dates, as this can affect the results.

In addition, using the `mktime()` function allows you to create a Unix timestamp, which can then be converted into a date using the `date()` function. This can be useful for comparing dates and performing more advanced calculations.

Overall, calculating dates in PHP may seem daunting at first, but with some practice and knowledge of the built-in Date and Time functions, it becomes a valuable skill to have in your programming toolbox.

## See Also
- [PHP Manual: Date and Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [W3Schools PHP Date and Time](https://www.w3schools.com/php/php_date.asp)
- [PHP DateTime class](https://www.php.net/manual/en/class.datetime.php)
---
title:                "Calculating a date in the future or past"
html_title:           "PHP recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculating a date in the future or past refers to determining a specific date, either in the future or the past, given a starting date and a number of days to add or subtract. Programmers often need to calculate dates for various tasks, such as scheduling events, creating countdowns, or managing recurring tasks.

## How to:
To calculate a date in the future or past in PHP, we can use the built-in `strtotime()` function. This function takes in two parameters - a string representation of a date and a number of days to add or subtract. Here's an example:

```PHP
$date = "2020-10-10"; //starting date
$days_to_add = 7; //number of days to add
$new_date = strtotime($date . " +" . $days_to_add . " days"); //calculates the new date
echo date("Y-m-d", $new_date); //outputs 2020-10-17
```

We can also specify a negative number of days to subtract from the starting date. This function supports various time formats as well, such as "next week", "next month", "next year", and "1 week ago". Here's another example:

```PHP
$date = "2020-12-01"; //starting date
$days_to_subtract = 14; //number of days to subtract
$new_date = strtotime($date . " -" . $days_to_subtract . " days"); //calculates the new date
echo date("m/d/Y", $new_date); //outputs 11/17/2020
```

## Deep Dive:
The `strtotime()` function in PHP is based on the Unix timestamp, which represents the number of seconds that have elapsed since January 1, 1970. It is important to note that the maximum value for this timestamp is likely to be in the year 2038, as it is stored as a signed 32-bit integer. This can cause issues when calculating dates far in the future, so alternative solutions such as using the DateTime class or external libraries may be necessary for more accurate results.

There are also other functions in PHP that can be used for date calculations, such as the `date_add()` and `date_sub()` functions, which allow for more specific date and time manipulations. Additionally, developers can also use the `DateTime` class, which offers more flexibility and built-in methods for date calculations.

When implementing date calculations in a project, it is important to consider the desired output format and the potential limitations of the chosen method to ensure accurate and efficient results.

## See Also:
- [PHP strtotime() function documentation](https://www.php.net/manual/en/function.strtotime.php)
- [DateTime class documentation](https://www.php.net/manual/en/datetime.format.php)
- [Alternative date calculation methods in PHP](https://www.geeksforgeeks.org/php-date-calculation-functions/)
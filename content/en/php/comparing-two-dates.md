---
title:                "PHP recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why 

Comparing two dates is a fundamental concept in PHP programming. It allows developers to determine which date comes before or after another, as well as calculate the time difference between two dates. This information can be useful in various scenarios such as event scheduling, data analysis, and more.

## How To 

To compare two dates in PHP, we can use the built-in function `strtotime()`. This function converts a date string into a UNIX timestamp, which is essentially a number representing the number of seconds since January 1st, 1970. We can then use the `>` (greater than) and `<` (less than) operators to compare the timestamps and determine which date comes first.

Let's see an example of how this works in code:

```PHP
$date1 = "2020-01-05";
$date2 = "2020-02-10";

$timestamp1 = strtotime($date1);
$timestamp2 = strtotime($date2);

if ($timestamp1 > $timestamp2) {
    echo "$date1 comes after $date2";
} else {
    echo "$date1 comes before $date2";
}
```

The code above will output `"2020-01-05 comes before 2020-02-10"`. This tells us that January 5th, 2020 comes before February 10th, 2020. We can also use the `==` (equal to) operator to check if two dates are the same.

Another important concept to keep in mind while comparing two dates is the time zone. When comparing two dates, it's important to make sure that they are in the same time zone. This can be achieved by setting the time zone using the `date_default_timezone_set()` function.

## Deep Dive 

As mentioned earlier, the `strtotime()` function converts a date string into a UNIX timestamp. This function also has the ability to handle various date formats, making it a powerful tool for comparing dates.

Some common date formats that `strtotime()` can handle are:

- `Y-m-d` (year-month-day)
- `m/d/Y` (month/day/year)
- `d-M-Y` (day-month-year)
- `F jS, Y` (month day, year)

In addition to the greater than, less than, and equal to operators, there are also other comparison operators that can be useful when working with dates. These include `>=` (greater than or equal to), `<=` (less than or equal to), and `!=` (not equal to).

## See Also

- PHP Date and Time functions: https://www.php.net/manual/en/ref.datetime.php
- Understanding UNIX Timestamps: https://www.unixtimestamp.com/
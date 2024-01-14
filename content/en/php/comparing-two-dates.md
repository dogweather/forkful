---
title:                "PHP recipe: Comparing two dates"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Most commonly, programmers encounter the need to compare two dates when working with date-based data such as event planning, payment schedules, or user activity. Being able to compare dates allows for efficient data manipulation and logical decision-making in PHP applications.

## How To
```PHP
// Defining two dates
$firstDate = "2020-05-01";
$secondDate = date("Y-m-d");

// Comparing if the first date is before the second date
if (strtotime($firstDate) < strtotime($secondDate)) {
    echo "The first date is before the second date.";
}

// Output: The first date is before the second date.
```

It is important to note that the `strtotime()` function converts the date string into a Unix timestamp, which is necessary for date comparisons in PHP. Here are some other examples of date comparisons:

```PHP
// Is one date equal to another date?
if (strtotime($firstDate) == strtotime($secondDate)) {
    // Do something
}

// Is one date after another date?
if (strtotime($firstDate) > strtotime($secondDate)) {
    // Do something
}

// Is one date within a certain range?
if (strtotime($firstDate) >= strtotime($startDate) && strtotime($firstDate) <= strtotime($endDate)) {
    // Do something
}
```

For more complex comparisons, there is also the `DateTime` class in PHP which allows for more precise control over date and time calculations.

## Deep Dive
Comparing dates may seem straightforward at first, but there are some key things to keep in mind. Firstly, the `strtotime()` function accepts a wide variety of date formats, but it is important to use a consistent format for accurate comparisons. It is also important to consider timezones when working with dates to avoid any inconsistencies.

Another aspect to consider is the concept of "fuzzy" comparisons. When using the comparison operators (`<`,`>`,`==`), PHP will automatically convert the date strings into Unix timestamps, which allows for easy comparisons. However, this means that the comparison is made at a specific point in time and does not take into account the time component of the dates. This can lead to unexpected results, especially when dealing with dates that include a time component. To avoid this, it is recommended to use the `DateTime` class or to convert both dates to Unix timestamps before making the comparison.

## See Also
- [PHP date and time functions](https://www.php.net/manual/en/ref.datetime.php)
- [PHP DateTime class documentation](https://www.php.net/manual/en/class.datetime.php)
- [Understanding Unix timestamps](https://www.epochconverter.com/)
- [Handling timezones in PHP](https://www.php.net/manual/en/timezones.php)
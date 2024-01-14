---
title:    "PHP recipe: Comparing two dates"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

When building a website or application, developers often need to compare two dates for various reasons. This could be for events scheduling, tracking user activities, or displaying time-sensitive information. Understanding how to compare dates in PHP is an essential skill for any programmer.

## How To

To compare two dates in PHP, we first need to convert them into a common format. The most common format for dates is the UNIX timestamp, which represents the number of seconds that have elapsed since January 1, 1970, at 00:00:00 UTC. We can use the `strtotime()` function to convert a date string into a UNIX timestamp.

Let's take a look at an example:

```PHP
// Two different dates
$date1 = "2021-06-01";
$date2 = "2021-06-15";

// Convert dates into UNIX timestamps
$timestamp1 = strtotime($date1);
$timestamp2 = strtotime($date2);

// Compare the timestamps
if ($timestamp1 < $timestamp2) {
    echo "$date1 is before $date2";
} elseif ($timestamp1 > $timestamp2) {
    echo "$date2 is before $date1";
} else {
    echo "The dates are the same";
}
```

In this example, we first define two different dates, `$date1` and `$date2`. Then, we use `strtotime()` to convert them into UNIX timestamps. Finally, we use a series of conditional statements to compare the timestamps and display the appropriate message.

The output of this code would be:

```
2021-06-01 is before 2021-06-15
```

We can also use other date functions, such as `date()` and `DateTime`, to compare dates in PHP. It's essential to consider the timezones when comparing dates to ensure accuracy.

## Deep Dive

When comparing dates, you may encounter situations where the dates have the same format, but one is stored in a string, while the other is stored in a DateTime object. In such cases, we can use the `date()` function to convert the DateTime object into a formatted string for comparison.

Here's an example:

```PHP
// Date string
$date1 = "2021-06-01";

// DateTime object
$date2 = new DateTime("2021-06-15");

// Convert DateTime object into formatted string
$date2 = date("Y-m-d", $date2->getTimestamp());

// Compare the dates
if ($date1 === $date2) {
    echo "The dates are the same";
} else {
    echo "The dates are different";
}
```

In this example, we use the `getTimestamp()` method to get the UNIX timestamp from the DateTime object, and then use `date()` to convert it into a formatted string. After that, we can compare the two dates using the `===` operator, which checks if the values and types are equal.

## See Also

Here are some helpful resources to learn more about comparing dates in PHP:

- [PHP Date and Time functions](https://www.php.net/manual/en/ref.datetime.php)
- [Codecademy PHP Date and Time Manipulation](https://www.codecademy.com/learn/learn-php/modules/learn-php-datetime)
- [PHP Manual - Comparing Dates](https://www.php.net/manual/en/datetime.diff.php)
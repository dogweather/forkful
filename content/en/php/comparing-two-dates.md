---
title:    "PHP recipe: Comparing two dates"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When developing a website or application, it's common to have to compare two dates. This could be for tasks like calculating the difference in time, checking for overlap, or sorting data chronologically. In this blog post, we'll explore how to compare two dates using PHP.

## How To

To compare two dates in PHP, we can use the built-in `DateTime` class. This class allows us to create date objects, perform operations on them, and compare them. Let's take a look at some examples.

First, we'll create two date objects for comparison:

```PHP
$date1 = new DateTime('2021-01-01');
$date2 = new DateTime('2021-01-15');
```

To compare these dates, we can use the `->diff()` method, which returns a `DateInterval` object. We can then access the `days` property of this object to get the number of days between the two dates:

```PHP
$diff = $date1->diff($date2);
echo $diff->days; // output: 14
```

We can also check if one date is greater than, less than, or equal to the other using the `->format()` method. This method allows us to specify a format for the date, and then compare the resulting string. For example:

```PHP
$date1 = new DateTime('2021-01-01');
$date2 = new DateTime('2021-01-15');
echo $date1->format('Y-m-d') < $date2->format('Y-m-d'); // output: true
```

Finally, we can also use the `->modify()` method to alter a date object. This can be useful for tasks like checking for date overlap. For example, if we want to check if the date range of `$date1` overlaps with the date range of `$date2`, we can modify the start and end dates of `$date2` and then compare them:

```PHP
$date1 = new DateTime('2021-01-01');
$date2 = new DateTime('2021-02-01');
$date2->modify('+14 days');
echo $date2 >= $date1 && $date2 <= $date2; // output: true
```

## Deep Dive

Under the hood, the `DateTime` class uses the Unix timestamps system, which counts the number of seconds since January 1, 1970. This is why we can compare dates as integers using the `->format()` method.

It's also worth noting that the `DateTime` class takes time zone into consideration when creating date objects. If no time zone is specified, the default time zone of the server will be used. This can lead to unexpected results when comparing dates, so it's important to set the time zone or use the `UTC` time zone for consistency.

## See Also

- [PHP DateTime class documentation](https://www.php.net/manual/en/class.datetime.php)
- [Unix timestamps](https://www.php.net/manual/en/function.time.php)
- [DateTime format options](https://www.php.net/manual/en/datetime.format.php)
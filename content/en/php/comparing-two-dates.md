---
title:                "Comparing two dates"
html_title:           "PHP recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to compare two dates in your programming projects? Maybe you need to check if an event has passed or if a user's account is still active. Whatever the reason may be, comparing two dates is a common task in the world of programming. In this article, we will explore how to compare two dates in PHP and why it can be useful in your projects.

## How To

To begin, let's look at how to compare two dates in PHP using the built-in `DateTime` class. First, we need to create two `DateTime` objects with the dates we want to compare:

```PHP
$firstDate = new DateTime('2021-04-01');
$secondDate = new DateTime('2021-04-15');
```

Now, let's compare these two dates using the `diff()` method:

```PHP
$dateDiff = $firstDate->diff($secondDate);
```

This will return a `DateInterval` object, which contains the difference between the two dates in terms of years, months, days, etc. We can then use the `days` property to get the number of days between the two dates:

```PHP
$days = $dateDiff->days;
echo "The difference between the two dates is $days days.";
```

We can also compare the two dates using the `DateTime` objects themselves. The `DateTime` class has a `format()` method which allows us to format the dates in a specific way. We can use this to compare the two dates by converting them to strings and then comparing them:

```PHP
$firstDateAsString = $firstDate->format('Y-m-d');
$secondDateAsString = $secondDate->format('Y-m-d');

if ($firstDateAsString === $secondDateAsString) {
    echo "The two dates are equal.";
} elseif ($firstDateAsString > $secondDateAsString) {
    echo "The first date is after the second date.";
} else {
    echo "The first date is before the second date.";
}
```

This will give us the flexibility to compare the dates in different formats and also perform different actions depending on the result of the comparison.

## Deep Dive

When comparing two dates in PHP, it is important to be aware of the time zone your server is set to. The `DateTime` class uses the default time zone set in your `php.ini` file. This means that if your server is set to a different time zone than the one specified in the dates, the comparison may not give the expected results. To avoid this, you can specify the time zone for each `DateTime` object like this:

```PHP
$firstDate = new DateTime('2021-04-01', new DateTimeZone('America/New_York'));
$secondDate = new DateTime('2021-04-15', new DateTimeZone('America/New_York'));
```

This will ensure that both `DateTime` objects are using the same time zone for the comparison.

Another thing to keep in mind is that when comparing two dates, the comparison is done based on the entire date and time, not just the day. This means that if your dates have different times, the comparison may not give the expected result. To compare only the dates and ignore the time, you can use the `setTime()` method to set the time of both `DateTime` objects to midnight before comparing them.

## See Also

- [PHP DateInterval Class](https://www.php.net/manual/en/class.dateinterval.php)
- [PHP DateTime Class](https://www.php.net/manual/en/class.datetime.php)
- [PHP DateTimeZone Class](https://www.php.net/manual/en/class.datetimezone.php)
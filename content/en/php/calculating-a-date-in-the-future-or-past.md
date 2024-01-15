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

## Why
Calculating dates in the future or past is a common task in web development, especially when creating features like event calendars or project timelines. It allows for better organization and planning of events or tasks.

## How To
To calculate a date in the future or past, we can use PHP's built-in `DateTime` class. We first create a new instance of the `DateTime` class, passing in the desired date and time. Then, we can use the `modify()` method to add or subtract a specific amount of time.

```PHP
// Calculate 10 days from today
$today = new DateTime();
$today->modify('+10 days');
echo $today->format('Y-m-d');
// Output: 2021-08-16

// Calculate 3 months and 2 weeks from a specific date
$specificDate = new DateTime('2022-02-10');
$specificDate->modify('+3 months +2 weeks');
echo $specificDate->format('Y-m-d');
// Output: 2022-06-24
```

We can also use the `DateTime` class to compare dates and determine the amount of time between them. The `diff()` method returns a `DateInterval` object, which can then be formatted to display the difference in days, weeks, months, etc.

```PHP
// Compare two dates and output the difference in days
$date1 = new DateTime('2021-07-20');
$date2 = new DateTime('2021-08-01');
$diff = $date1->diff($date2);
echo $diff->format('%a days');
// Output: 12 days
```

## Deep Dive
The `DateTime` class also allows for more complex calculations, such as finding the last day of a specific month or adjusting for daylight saving time. It also takes into account leap years when calculating dates.

```PHP
// Get the last day of a specific month
$date = new DateTime('2020-02-01');
$date->modify('last day of this month');
echo $date->format('Y-m-d');
// Output: 2020-02-29

// Adjust for daylight saving time
$date = new DateTime('2021-03-14', new DateTimeZone('America/New_York'));
$date->modify('+1 day');
echo $date->format('Y-m-d');
// Output: 2021-03-15 (due to daylight saving time, the date is automatically adjusted one hour ahead)
```

Keep in mind that the `DateTime` class only supports dates within the range of 1970-01-01 to 2038-01-19 due to limitations in the underlying system. For dates outside of this range, consider using the `DateTimeImmutable` class or a third-party library.

## See Also
- PHP Manual on `DateTime` class: https://www.php.net/manual/en/class.datetime.php
- Date and Time functions in PHP: https://www.php.net/manual/en/ref.datetime.php
- Carbon PHP library for extended date and time functionality: https://carbon.nesbot.com/
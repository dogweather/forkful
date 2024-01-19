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
Manipulating dates, like calculating a future or past date, allows programmers to schedule events, deadlines, or simply add time-based functionality. Accurate date calculations are crucial in systems from event reminders to premium subscription management.

## How to:
In PHP, manipulating dates is intuitive and effective. Here's an example of calculating future dates:

```PHP
$today = new DateTime();
$futureDate = $today->modify('+1 month');
echo $futureDate->format('Y-m-d');
```

In this code snippet, we're creating a DateTime object for the current date (`$today`), modifying it to a month in the future, and echoing the future date in `YYYY-MM-DD` format. Change '+1 month' to any period you need, like '+1 day', '+5 years', '-15 minutes', etc.

## Deep Dive
Historically in PHP, date calculations relied on UNIX timestamps, limiting the dates to a range from 1970-01-01 to 2038-01-19. With the introduction of the DateTime class in PHP 5.2.0, calculations became more flexible and could handle a wider range of dates (0001-01-01 to 9999-12-31).

An alternative method is to use `strtotime`, a powerful function that parses most English textual datetime descriptions into a Unix timestamp.

```PHP
$futureDate = strtotime("+1 month", time());
echo date('Y-m-d', $futureDate);
```

Remember that `strtotime` is susceptible to the "Year 2038" problem on 32-bit PHP installations.

Behind the scenes, the DateTime class uses the system's own timezone settings for calculations. You can override this by setting the optional timezone parameter when creating a DateTime object.

```PHP
$today = new DateTime(null, new DateTimeZone('Europe/London'));
...
```

## See Also
- PHP.net documentation on DateTime class: [https://www.php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- How to use date and time in PHP: [https://dzone.com/articles/how-to-use-date-and-time-in-php](https://dzone.com/articles/how-to-use-date-and-time-in-php)
- Explaining the "Year 2038" problem: [https://en.wikipedia.org/wiki/Year_2038_problem](https://en.wikipedia.org/wiki/Year_2038_problem)
- List of supported timezones in PHP: [https://www.php.net/manual/en/timezones.php](https://www.php.net/manual/en/timezones.php)
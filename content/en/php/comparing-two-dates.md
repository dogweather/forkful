---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Similar to comparing numbers or strings, comparing two dates in programming is about determining which date is earlier, later, or if they are the same. Programmers often do this in tasks involving scheduling, sorting data by date, or measuring time intervals.

## How To:

PHP offers several ways to compare dates, but the most straightforward approach involves its built-in DateTime class. Below is a basic date comparison method.

```PHP
<?php
$date1 = new DateTime('2021-01-01');
$date2 = new DateTime('2022-01-01');

if ($date1 > $date2) {
    echo "Date1 is later.";
} else if ($date1 < $date2) {
    echo "Date2 is later.";
} else {
    echo "Both dates are the same.";
}
?>
```

If you run the above snippet, it will output:

```
Date2 is later.
```

## Deep Dive:

Comparing dates in PHP became significantly easier with version 5.2.0 in 2006 when DateTime class was introduced. Before it, programmers used functions like `strtotime()`, then compared their Unix timestamps – an approach still available today.

While DateTime class has proven sufficient for everyday needs, PHP also provides alternatives like `date_diff()` for more specific tasks such as finding the difference between two dates.

Under the hood, when two DateTime objects are compared, PHP actually compares their Unix timestamps. It's a great reminder that regardless of how dates might appear in your database or user interface, PHP (and most server-side languages) treat dates as numbers behind the scenes.

## See Also:

For more details on working with dates in PHP:

1. [DateTime Documentation](https://www.php.net/manual/en/class.datetime.php)
2. [Date/Time Functions](https://www.php.net/manual/en/ref.datetime.php)
3. [PHP: The Right Way – Date and Time](https://phptherightway.com/#date_and_time)
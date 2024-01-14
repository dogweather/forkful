---
title:                "PHP recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is a common task in programming, especially when working with events or scheduling. Being able to accurately determine a date in the future or past allows for better organization and planning in various projects.

## How To
To calculate a date in the future or past, you will need to start with the current date and then use PHP's date functions to manipulate it. For example, if you want to calculate a date 5 days in the future, you can use the `strtotime()` function to add 5 days to the current date.

```
<?php

$current_date = date('Y-m-d'); // Get current date
$future_date = date('Y-m-d', strtotime('+5 days', strtotime($current_date))); // Calculate date 5 days in the future
echo $future_date; // Output: 2021-07-06
```
Similarly, if you want to calculate a date in the past, you can use the `strtotime()` function with a negative value. For instance, to get the date 2 weeks ago, you can use `-2 weeks` as the second parameter.

```
<?php

$current_date = date('Y-m-d'); // Get current date
$past_date = date('Y-m-d', strtotime('-2 weeks', strtotime($current_date))); // Calculate date 2 weeks ago
echo $past_date; // Output: 2021-06-15
```

## Deep Dive
PHP offers a variety of date functions that can be used to perform different calculations. Some of the commonly used ones include `strtotime()`, `date()`, `strtotime()` and `mktime()`. It's important to pay attention to the format of the date when using these functions as it can affect the result.

Another important thing to note is that PHP's date functions are dependent on the server's time zone. It's recommended to set the correct time zone in your PHP settings to avoid any discrepancies in date calculations.

## See Also
- PHP date functions: https://www.php.net/manual/en/ref.datetime.php
- Formatting date and time in PHP: https://www.php.net/manual/en/function.date.php
- Setting time zone in PHP: https://www.php.net/manual/en/function.date-default-timezone-set.php
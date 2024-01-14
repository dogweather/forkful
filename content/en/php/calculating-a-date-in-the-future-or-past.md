---
title:    "PHP recipe: Calculating a date in the future or past"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to calculate a date that is either in the future or in the past? Perhaps for scheduling events, setting reminders, or planning projects? In this blog post, we will explore how to use PHP to easily calculate and manipulate dates.

## How To
Let's start with a simple task - finding the date that is 10 days from now. In PHP, we can use the `strtotime()` function to convert a string into a Unix timestamp. For example, if we use the string "now" as an argument, it will return the current timestamp. So to get the timestamp for 10 days from now, we can use the `+` sign followed by the number of days. The code would look like this:

```PHP
$future_timestamp = strtotime("+10 days");
echo date("Y-m-d", $future_timestamp);
```

The `date()` function allows us to format the timestamp into a readable date. In this case, we specified the format as Year-Month-Day (Y-m-d). The output would be a date that is 10 days from the current date. Pretty simple, right?

But what if we need to find a date in the past? We can use the same `strtotime()` function but add a `-` sign before the number of days. For example, if we want to find a date that is 1 month ago, the code would look like this:

```PHP
$past_timestamp = strtotime("-1 month");
echo date("Y-m-d", $past_timestamp);
```

We can use this method to calculate dates in the future or in the past by changing the number of days or even using other time units like weeks, years, hours, etc.

## Deep Dive
Now that we know how to calculate dates, let's dive deeper into manipulating them. PHP has a built-in `DateTime` class that allows us to easily work with dates. We can create a `DateTime` object with a specific date and perform operations on it.

For example, if we want to find the date that is 3 months after a specific date, we can use the `modify()` method to add 3 months to the date. The code would look like this:

```PHP
$date = new DateTime("2021-06-15");
$date->modify("+3 months");
echo $date->format("Y-m-d");
```

We can also use the `diff()` method to find the difference between two dates. For example, let's say we want to find the number of days between two specific dates. The code would look like this:

```PHP
$start_date = new DateTime("2021-06-01");
$end_date = new DateTime("2021-06-15");
$diff = $start_date->diff($end_date);
echo $diff->format("%a");
```

The `format()` method allows us to specify the format of the output. In this case, we use `%a` to get the number of days as the output.

## See Also
Here are some useful resources for working with dates in PHP:

- [PHP Manual: Date and Time Functions](https://www.php.net/manual/en/book.datetime.php)
- [W3Schools: PHP Date and Time](https://www.w3schools.com/php/php_date.asp)
- [PHP: Calculating future or past dates](https://www.php.net/manual/en/datetime.modify.php)
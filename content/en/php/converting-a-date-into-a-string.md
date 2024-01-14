---
title:    "PHP recipe: Converting a date into a string"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why 

Converting dates into strings is a common task in web development. It allows us to display dates in a format that is more user-friendly and customizable. This can be helpful for things like event calendars or blog posts where the date needs to be formatted in a specific way.

## How To

To convert a date into a string in PHP, we can use the `date()` function. This function takes two parameters - a format and a timestamp. The format specifies how we want the date to be displayed, and the timestamp is the date we want to convert. Let's see an example:

```PHP
$date = date("F j, Y", strtotime("2020-09-20"));
echo $date;

// Output: September 20, 2020
```

In this example, we are using the `date()` function to convert the date "2020-09-20" into a string in the format "F j, Y" which stands for full textual month, followed by day of the month and year. We could also use other formatting options such as "m/d/Y" or "d-m-Y" depending on our preferences.

## Deep Dive

When using the `date()` function, it's essential to understand the formatting options and how they are represented. In the above example, we used "F j, Y" for our format, but there are many other options available. For example, if we want to display the day of the week, we can use "l" in our format, or if we want to include the time as well, we can use "g:i a" for hours and minutes in 12-hour format with "am" or "pm".

Additionally, we can use the `strtotime()` function to convert different date formats into a timestamp. We can also use the `strtotime()` function to add or subtract days, months, or years from our timestamp. This makes it easier to manipulate dates and display them in different formats.

## See Also 

- [PHP date() function documentation](https://www.php.net/manual/en/function.date.php)
- [PHP strtotime() function documentation](https://www.php.net/manual/en/function.strtotime.php)
- [PHP date formats cheat sheet](https://www.php.net/manual/en/datetime.format.php)
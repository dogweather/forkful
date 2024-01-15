---
title:                "Converting a date into a string"
html_title:           "Gleam recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why 
Dates are a fundamental part of our daily lives, and as programmers, we often need to manipulate them in various formats. One common task is converting a date into a string. This article will explore how to do this in Gleam, a modern and powerful programming language.

## How To 
To convert a date into a string in Gleam, we will use the `Time` module from the standard library. First, we need to import the module using the `import` keyword:

```Gleam
import gleam/time
```

Next, we can use the `format` function from the `Time` module to convert our date into a string:

```Gleam
let date = Time.Date.new(2021, 9, 1)
let formatted_date = Time.format("%Y-%m-%d", date)
```

In this example, we create a new `Date` object representing September 1st, 2021, and then use the `%Y-%m-%d` format string to convert it into a string in the format of year-month-day. The resulting `formatted_date` will be a string with the value of "2021-09-01".

You can also specify different formats to suit your needs, such as `%b %d, %Y` for abbreviated month name, day, and year (e.g. Sep 01, 2021) or `%A, %B %e` for the full weekday name, full month name, and day (e.g. Wednesday, September 1).

## Deep Dive 
Now that we have a basic understanding of how to convert a date into a string in Gleam, let's take a closer look at the `format` function. It takes in two arguments - a format string and the date to be formatted. The format string contains specific characters that represent different parts of the date, such as year, month, day, hour, minute, second, etc. By using the appropriate format characters, we can control how the final string will look.

One important thing to note is that the `format` function uses the ISO 8601 standard for formatting dates, which is widely accepted and easily understandable. This standard specifies the use of four-digit years, two-digit months and days, and the use of dashes or colons as separators.

If needed, Gleam also offers the `strftime` function from the `Os` module, which allows for customizable formatting using the POSIX strftime formatting standard.

## See Also 
To learn more about working with dates in Gleam, check out the following resources: 
- [Gleam Time Module Documentation](https://gleam.run/modules/time.html)
- [ISO 8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)
- [POSIX strftime Format Codes](https://pubs.opengroup.org/onlinepubs/009695399/functions/strftime.html)
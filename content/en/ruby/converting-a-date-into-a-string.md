---
title:    "Ruby recipe: Converting a date into a string"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

When working with dates in Ruby, it is often necessary to convert them into strings for various purposes such as display or storage. By converting a date into a string, you can easily manipulate and format it to meet your needs.

## How To

Converting a date into a string in Ruby is a simple process. Let's take a look at an example using the `strftime` method.

```Ruby
date = Date.today
# => #<Date: 2021-08-25 ((2459452j,0s,0n),+0s,2299161j)>
date.strftime("%B %d, %Y")
# => "August 25, 2021"
```

In this example, we first create a new Date object with today's date. Then, we use the `strftime` method with a format string to convert the date into a string in the desired format. The `%B` represents the full month name, `%d` represents the day of the month, and `%Y` represents the full year. You can use various format directives to customize the output based on your needs.

## Deep Dive

When using the `strftime` method, the format directives are based on the strftime() function from the C language. The following table lists some commonly used directives and their meanings:

| Directive | Output |
| --- | --- |
| %Y | Full year |
| %m | Month number (01-12) |
| %B | Full month name |
| %d | Day of the month (01-31) |
| %A | Full weekday name |
| %j | Day of the year (001-365) |

It is important to note that the `strftime` method only works with Date objects in Ruby. If you try to use it with a Time or DateTime object, it will result in an error.

## See Also

- [Date and Time Class in Ruby](https://ruby-doc.org/stdlib-2.6.0/libdoc/date/rdoc/Date.html)
- [Format Directives for Date and Time in Ruby](https://www.rubyguides.com/2015/05/ruby-date-format/)
- [The strftime() function in C](https://www.programiz.com/c-programming/library-function/time/strftime)
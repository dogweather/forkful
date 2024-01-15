---
title:                "Converting a date into a string"
html_title:           "Python recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string is a common task in Python programming. A string representation of a date is useful for displaying dates in a human-readable format and for data manipulation and analysis.

## How To
The process of converting a date into a string involves using the built-in `strftime()` method. This method takes in a specified date format and returns a string representation of the date.

To begin, we need to import the `datetime` module:

```Python
import datetime
```

Next, we can create a `datetime` object using the `datetime()` constructor and specify the desired date:

```Python
my_date = datetime.datetime(2021, 8, 12)
```

Now, we can use the `strftime()` method to convert the date into a string in a specific format. Let's say we want the date to be displayed as "Month/Day/Year", we can use the format string "%m/%d/%Y" within the `strftime()` method:

```Python
my_string = my_date.strftime("%m/%d/%Y")
```

The variable `my_string` now holds the string representation of our date: "08/12/2021".

We can also include additional information such as the day of the week or the time of day in our string format. For example, using "%A, %I:%M %p" will give us a string like "Thursday, 04:30 PM".

## Deep Dive
The `strftime()` method allows us to customize the format of our date string using format codes. Some commonly used format codes are:

- %Y - year with century
- %m - month as a zero-padded decimal number
- %d - day of the month as a zero-padded decimal number
- %H - hour (24-hour clock) as a zero-padded decimal number
- %M - minute as a zero-padded decimal number
- %S - second as a zero-padded decimal number

These are just a few examples, and there are many more options available. You can find a complete list of format codes in the [Python datetime documentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes).

It's also worth noting that the `strftime()` method is not limited to only `datetime` objects. It can also be used with `date` and `time` objects, allowing for even more flexibility in date string formatting.

## See Also
- [Python datetime documentation](https://docs.python.org/3/library/datetime.html)
- [Python strftime reference](https://www.programiz.com/python-programming/datetime/strftime)
- [Python date and time tutorial](https://realpython.com/python-datetime/)

Happy coding!
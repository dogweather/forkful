---
title:    "Python recipe: Getting the current date"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

As life becomes increasingly digital, keeping track of time and date has become an essential part of our daily routines. From scheduling appointments to managing deadlines, knowing the current date is crucial for staying organized and on top of our tasks. And with the Python programming language, getting the current date is quick and easy.

## How To

To get the current date in Python, we can use the built-in `date` class from the `datetime` module. This class allows us to create objects that represent specific dates. Let's take a look at the following code:

```Python
from datetime import date

current_date = date.today()
print(current_date)
```

This code first imports the `date` class from the `datetime` module. Then, we use the `today()` method to create a `date` object that represents the current date. Finally, we use the `print()` function to display the value of `current_date`.

The output of this code would be the current date in the following format:

`yyyy-mm-dd`

For example, if today's date is January 20th, 2022, then the output would be:

`2022-01-20`

We can also customize the format of the output by using the `strftime()` method. This method allows us to specify a format string to display the date in a different format. Let's see an example:

```Python
print(current_date.strftime("%B %d, %Y"))
```

Here, we use the `%B` directive to display the full month name, `%d` for the day of the month, and `%Y` for the full year. The output of this code would be:

`January 20, 2022`

## Deep Dive

The `date` class in Python allows us to access various attributes and methods to work with dates. Some commonly used attributes include `year`, `month`, and `day`, which allow us to get the specific values of the date object.

Additionally, we can perform arithmetic operations on date objects. For example, we can use the `timedelta()` class to add or subtract a certain number of days from a given date. This can be useful for calculating future or past dates.

## See Also

- [Python documentation on the `date` class](https://docs.python.org/3/library/datetime.html#date-objects)
- [Python strftime directives](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
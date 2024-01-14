---
title:                "Python recipe: Comparing two dates"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how to compare two dates in Python? Whether you're working on a project that requires handling time-sensitive data or simply want to learn more about date and time operations in Python, comparing dates is a useful skill to have.

## How To

To compare two dates in Python, we can use the built-in `datetime` module. First, we need to import the module:

```Python 
import datetime
```

Next, we can create two `datetime` objects with the dates we want to compare. For example, let's compare today's date with a specific date in the past:

```Python 
today = datetime.datetime.today() 
past_date = datetime.datetime(2020, 1, 1)
```

We can then use comparison operators such as `>` (greater than), `<` (less than), `==` (equal to), etc. to compare the two dates. For example:

```Python 
print(today > past_date) 
# Output: True 
```

We can also use the `date()` method to only compare the date portion and ignore the time. For example, let's compare the dates without considering the time:

```Python 
today = datetime.datetime.today().date() 
past_date = datetime.datetime(2020, 1, 1).date()
print(today == past_date) 
# Output: True
```

## Deep Dive

When comparing dates, it's important to note that the `datetime` objects must be in the same format for the comparison to be accurate. This means that the year, month, day, hour, minute, and second must be in the same order for both dates.

Additionally, the `datetime` module provides various methods that can be used to manipulate and format dates. These methods can be useful when working with dates and comparing them. Some examples include `strftime()` for formatting dates into strings, `timedelta()` for adding or subtracting time, and `replace()` for modifying specific date components.

See the [official documentation](https://docs.python.org/3/library/datetime.html) for a full list of methods and their usage.

## See Also

For more information on working with dates and times in Python, check out these resources:

[Real Python: Working with Dates and Times in Python](https://realpython.com/python-datetime/)
[Python for Beginners: Dates and Times Tutorial](https://www.pythonforbeginners.com/basics/python-datetime-time-examples)
[Python Tutorial: Date and Time in Python](https://www.tutorialspoint.com/python/date_time.htm)
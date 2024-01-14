---
title:    "Python recipe: Calculating a date in the future or past"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to calculate a future or past date for a project or event? Maybe you want to know what day of the week your birthday will fall on next year, or what date your vacation will start in three months. Whatever the reason may be, being able to calculate dates in the future or past can be a useful tool for planning and organizing.

## How To

Python has a built-in module called `datetime` which makes it easy to calculate dates in the future or past. To get started, we need to import the `datetime` module:

```Python
import datetime
```

### Calculating a future date

To calculate a future date, we will use the `datetime.timedelta(days)` method, which takes in the number of days we want to add to the current date. Let's say we want to know what date will be 100 days from now, we can do it like this:

```Python
future_date = datetime.date.today() + datetime.timedelta(100)

print(future_date)
```

Output: 2021-08-06 (assuming today is April 28, 2021)

We can also specify a specific date to start with and add days to that date. For example, if we want to know what date will be 200 days from January 1, 2020, we can do it like this:

```Python
specific_date = datetime.date(2020, 1, 1)

future_date = specific_date + datetime.timedelta(200)

print(future_date)
```

Output: 2020-07-19

### Calculating a past date

Calculating a past date is similar to calculating a future date, except we will use a negative value for the `days` argument in the `timedelta` method. Let's say we want to know what date was 2 weeks ago from today, we can do it like this:

```Python
past_date = datetime.date.today() + datetime.timedelta(-14)

print(past_date)
```

Output: 2021-04-14

We can also specify a specific date and subtract days from that date. For example, if we want to know what date was 100 days before December 25, 2019, we can do it like this:

```Python
specific_date = datetime.date(2019, 12, 25)

past_date = specific_date + datetime.timedelta(-100)

print(past_date)
```

Output: 2019-09-16

## Deep Dive

The `datetime` module also allows us to perform more complex calculations, such as finding the difference between two dates. We can do this by subtracting one date from another, and the result will be a `timedelta` object representing the difference in days.

```Python
date1 = datetime.date(2021, 4, 28)
date2 = datetime.date(2021, 4, 20)

date_diff = date1 - date2

print(date_diff.days)
```

Output: 8

We can also perform other operations on `timedelta` objects, such as adding or subtracting time units like hours, minutes, or seconds. This makes it a useful tool for performing time-related calculations in our projects.

## See Also

- [Python datetime module documentation](https://docs.python.org/3/library/datetime.html)
- [Calculating future and past dates with Python](https://realpython.com/python-datetime/)
- [Date and time in Python](https://www.programiz.com/python-programming/datetime)
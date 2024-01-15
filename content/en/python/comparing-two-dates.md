---
title:                "Comparing two dates"
html_title:           "Python recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to compare two dates in your Python code? Maybe you're building a program that tracks events or reminders, and you need to know if one date comes before or after another. Being able to compare dates is an essential skill in many programming projects.

## How To

The good news is that comparing two dates in Python is simple and straightforward. Let's take a look at a few examples using the built-in `datetime` module.

To start, we'll create two `datetime` objects representing different dates:

```Python
import datetime

date1 = datetime.datetime(2021, 3, 15)
date2 = datetime.datetime(2021, 3, 20)
```

We can now compare these two dates using the comparison operators `<` and `>`. These operators return a boolean value indicating if the first date is before or after the second date:

```Python
print(date1 < date2)  # Output: True
print(date1 > date2)  # Output: False
```

We can also check if two dates are equal using the `==` operator:

```Python
print(date1 == date2)  # Output: False
```

But what if we want to compare the dates based on their time, not just their date? We can use the `.time()` method to access the time component of a `datetime` object:

```Python
date3 = datetime.datetime(2021, 3, 15, 8, 0, 0) # 8 AM on March 15th

print(date1.time() < date3.time())  # Output: True
```

In this example, we are comparing only the time component, so `date3` is considered to be before `date1`.

## Deep Dive

Under the hood, the comparison operators are using the `__lt__` (less than), `__gt__` (greater than), and `__eq__` (equal) methods of the `datetime` class to perform the comparisons. These methods allow us to customize how dates are compared if needed.

Additionally, the `datetime` module includes a handy `timedelta` class that allows us to compare the difference between two dates in terms of days, hours, minutes, etc. For example, we can find the number of days between `date1` and `date2` using the following code:

```Python
delta = date2 - date1
print(delta.days)  # Output: 5
```

## See Also

- [Python datetime documentation](https://docs.python.org/3/library/datetime.html)
- [Python timedelta documentation](https://docs.python.org/3/library/datetime.html#timedelta-objects)
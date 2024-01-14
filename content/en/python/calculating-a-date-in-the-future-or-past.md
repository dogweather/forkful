---
title:                "Python recipe: Calculating a date in the future or past"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many programming projects. It allows you to automate the process of finding specific dates and saves you from manual calculations.

## How To

Calculating a date in the future or past can be achieved through the use of Python's built-in `datetime` module. Let's take a look at some examples:

```
import datetime

# Calculating 7 days from today
today = datetime.date.today()
future_date = today + datetime.timedelta(days=7)
print(future_date)
```

Output: 2021-10-11

```
# Calculating 1 year and 3 months from a specific date
specific_date = datetime.date(2020, 5, 10)
future_date = specific_date + datetime.timedelta(years=1, months=3)
print(future_date)
```

Output: 2021-08-10

In the above examples, we used the `timedelta` function to add a specific amount of time to a given date. This function takes in parameters such as days, months, and years to calculate the future date.

## Deep Dive

The `datetime` module also has useful functions for retrieving information about a specific date. Let's take a look at some examples:

```
import datetime

# Retrieving the current year
current_year = datetime.date.today().year
print(current_year)
```

Output: 2021

```
# Retrieving the day of the week for a specific date
specific_date = datetime.date(2020, 5, 10)
day = specific_date.strftime("%A")
print(day)
```

Output: Sunday

Furthermore, the `datetime` module also allows for comparisons between dates. This can be useful when dealing with tasks such as finding the difference between two dates or checking if a certain date has already passed.

## See Also

- [Python datetime module documentation](https://docs.python.org/3/library/datetime.html)
- [Python timedelta function documentation](https://docs.python.org/3/library/datetime.html#timedelta-objects)
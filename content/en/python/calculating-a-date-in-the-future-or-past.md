---
title:                "Calculating a date in the future or past"
html_title:           "Python recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Article

## What & Why?

Manipulating dates and time, like calculating a future or past date, is a common task in programming. This technique is vital for scheduling events, like reminders or deadlines, and in data analysis tasks.

## How to:

Python's built-in `datetime` module simplifies work with dates and time. Here's how to calculate a future date:

```Python
import datetime

today = datetime.date.today()
future_date = today + datetime.timedelta(days=30)

print(future_date)
```
This code prints the date 30 days from the current date. For a past date:

```Python
past_date = today - datetime.timedelta(days=30)

print(past_date)
```

The `datetime.timedelta` function helps calculate a date relative from another date here. 

## Deep Dive

#### Historical context:

In the past, programmers would manually handle edge cases like leap years or different month lengths. This was tedious and error-prone, leading to the creation of dedicated date-time libraries.

#### Alternatives:

Besides `datetime`, Python also provides `dateutil`. It has advanced features like parsing dates from strings and handling timezone conversions:

```Python
from dateutil.relativedelta import relativedelta

future_date = today + relativedelta(months=+1)
```

The `Pandas` library is helpful when working with large datasets.

#### Implementation details:

Behind the scenes, `datetime` objects are fundamentally a timestamp from a standard reference point - the Unix epoch (1970-01-01). The timedelta is simply an offset in seconds. 

## See Also

For more details, check these resources: 

1. [Python's datetime documentation](https://docs.python.org/3/library/datetime.html)
2. [Python's dateutil documentation](https://dateutil.readthedocs.io/en/stable/)
3. [Python's Pandas library documentation](https://pandas.pydata.org/docs/)
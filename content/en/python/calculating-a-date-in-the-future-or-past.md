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

## What & Why?

Calculating a date in the future or past involves manipulating a given date by specific days, weeks, months, or years. Programmers do this to handle time-based data, enabling tasks like scheduling events, setting deadlines, or determining intervals.

## How To:

Python’s built-in `datetime` module does the job. Let's create a date and then subtract or add days to it.

```
import datetime

# Current date
today = datetime.date.today()
print("Today's date:", today)

# Add 5 days to current date
future_date = today + datetime.timedelta(days=5)
print("Date 5 days from today:", future_date)

# Subtract 2 weeks from current date
past_date = today - datetime.timedelta(weeks=2)
print("Date 2 weeks ago from today:", past_date)
```

When you run the code, it might output something like:

```
Today's date: 2022-03-30
Date 5 days from today: 2022-04-04
Date 2 weeks ago from today: 2022-03-16
```

## Deep Dive

Python's `datetime` module has been around since version 2.3, making it a reliable choice for date calculations. However, if you want a more powerful tool, consider `dateutil.relativedelta`. It provides a more flexible way to perform date arithmetic, including the ability to jump forward or backward by a number of weekdays.

However, these methods only work with standard Gregorian calendar dates. For alternative calendar systems or more extensive date manipulation, look into libraries like `pendulum`, `arrow`, or `moment`.

Here's the code again, but this time, using `relativedelta`:

```
from datetime import datetime
from dateutil.relativedelta import relativedelta

# Current date
today = datetime.now()

# Add 2 months to current date
future_date = today + relativedelta(months=2)
print("Date 2 months from now:", future_date)

# Subtract 1 year from current date
past_date = today - relativedelta(years=1)
print("Date 1 Year ago from now:", past_date)
```

## See Also:

- Python's official [`datetime`](https://docs.python.org/3/library/datetime.html) documentation.
- The [`dateutil`](https://dateutil.readthedocs.io/en/stable/) library documentation.
- For more advanced date manipulation, check out [`pendulum`](https://pendulum.eustace.io/docs/), [`arrow`](https://arrow.readthedocs.io/en/latest/), and [`moment`](https://moment.readthedocs.io/en/latest/).
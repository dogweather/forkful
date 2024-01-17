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
Calculating a date in the future or past refers to the process of determining a specific date that is either ahead of or behind a given date. In programming, this is often done to track deadlines, schedule tasks, or perform calculations based on specific timeframes.

Programmers use date calculations to automate tasks and create reliable systems that require specific actions to take place on certain dates. This eliminates the need for manual calculations and reduces the risk of human error. 

## How to:
Calculating a date in the future or past can be achieved in Python using the datetime module. 

To calculate a future date, you can use the timedelta object to add a specific number of days, months, or years to a given date. For example, if we want to get the date 90 days from today, we can use the following code:

```Python
import datetime
today = datetime.date.today()
future_date = today + datetime.timedelta(days=90)
print(future_date)
```

Output:
```
2021-10-25
```

To calculate a past date, we can use the same timedelta object but with a negative value. For instance, to get the date 2 weeks before today, we can use the following code:

```Python
import datetime
today = datetime.date.today()
past_date = today - datetime.timedelta(weeks=2)
print(past_date)
```

Output:
```
2021-07-12
```

## Deep Dive:
Calculating dates has been an essential aspect of computing since the early days. In the past, programmers had to write complex code to perform date calculations. However, with the introduction of modules like datetime in Python, this process has become much more straightforward and efficient.

An alternative way of calculating dates in Python is by using the calendar module. This module provides functions for working with dates, including determining leap years, the number of days in a month, and the day of the week for a given date. While it offers more flexibility, it requires more complex code compared to using the datetime module.

Behind the scenes, the datetime module uses a timestamp, which is the number of seconds elapsed since the Unix epoch (January 1, 1970). This makes date calculations more accurate and efficient.

## See Also:
- Python datetime module documentation: https://docs.python.org/3/library/datetime.html
- Python calendar module documentation: https://docs.python.org/3/library/calendar.html
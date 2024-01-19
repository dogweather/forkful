---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates means finding their relative temporal positions. Programmers use this to trigger certain actions, calculate durations, or organize data chronologically.

## How to:

Let's consider two dates, `date1` and `date2`, and compare using Python built-in datetime module.

```python
from datetime import datetime

# defining two dates
date1 = datetime(2022, 4, 17)
date2 = datetime(2022, 5, 17)

# comparing dates
if date1 < date2:
    print("date1 is earlier")
elif date1 == date2:
    print("Both dates are the same")
else:
    print("date2 is earlier")
```
You'll see "date1 is earlier".

## Deep Dive:

Python's built-in datetime module emerged in version 2.3, making date comparison a breeze. There're alternatives like time module for epoch time comparisons, or third-party packages like dateutil.

DateTime comparison actually compares the dates' timestamp values numerically. It's worth noting that Python compares dates/times to the microsecond. 

## See Also:

Python's datetime documentation [here](https://docs.python.org/3/library/datetime.html)

[Python's time module documentation](https://docs.python.org/3/library/time.html)

[dateutil's PyPi documentation](https://pypi.org/project/python-dateutil/)
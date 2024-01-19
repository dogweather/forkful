---
title:                "Getting the current date"
html_title:           "Python recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in programming is about retrieving today's date. Programmers usually do this to track real-time events, log timestamps, or calculate time intervals.

## How To:

In Python, the built-in `datetime` module makes accessing the current date dirt simple. Here's how:

```Python
import datetime

# Get today's date
today = datetime.date.today()

print(today)
```
You run the code, it'll spit out something similar:
```Python
2021-10-14  
```
It's simply the Year-Month-Day. Pretty straightforward, right?

## Deep Dive

Historically, manipulating dates and times wasn't a side job. Several languages devised their own models, some clunkier than others. Python smoothly traversed this history, and the `datetime` module we use today is the result.

There are alternatives. Libraries like `time` or `calendar` can also get the job done. Yet, they lack the simplicity and functionality that `datetime` offers.

Under the hood, `datetime` works by interfacing with the system clock, retrieving data, and formatting it to a human-friendly output. Remember, Python prides itself on being user-friendly and readable. That's why `datetime.date.today()` simply makes sense.

## See Also

For more details, head over to the Python docs:
[datetime — Basic date and time types](https://docs.python.org/3/library/datetime.html)

For advanced date/time handling, check out third-party libraries like Arrow:
[Arrow: Better dates & times for Python](https://arrow.readthedocs.io/en/latest/)
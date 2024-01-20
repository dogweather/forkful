---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in Python is about fetching the real-time date. It's used in timestamping data, tracking events, or performing operations based on the current date.

## How to:

The `datetime` library makes it a breeze. Just import it and use `datetime.date.today()`. Like so:

```Python
import datetime

current_date = datetime.date.today()

print(current_date)
```

If you run this, you'll see something like:

```Output
2022-02-22
```

And that's your current date!

## Deep Dive

This way of getting the current date in Python leans on the `date` class of the `datetime` module, standard since Python 2.3. 

There's extra ways to get the same info. For instance, you can get the current date and time together using `datetime.datetime.now()`:

```Python
import datetime

current_datetime = datetime.datetime.now()

print(current_datetime)
```

It will show something like:

```Output
2022-02-22 14:01:22.800600
```

In the output, '14:01:22.800600' represents the current time (in hours, minutes, seconds, and microseconds).

Internally, `datetime` handles dates as a Gregorian calendar. This way, it supports dates from year 1 to 9999, which is more than enough for most of us!

## See Also

For more detail about the `datetime` module, scope out the [Python docs](https://docs.python.org/3/library/datetime.html). Also check out the official Python tutorial's chapter on [dates and times](https://docs.python.org/3/tutorial/stdlib.html#dates-and-times). Happy coding!
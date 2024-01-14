---
title:    "Python recipe: Getting the current date"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know what day it is? Whether it's for a project, a meeting, or just to keep track of the passing of time, knowing the current date is a useful piece of information. Luckily, it's easy to get the current date using Python programming. In this blog post, we will explore how to retrieve the current date and some interesting ways to use it.

## How To

In Python, we can use the `datetime` module to get the current date. First, we need to import the module:

```python
import datetime
```

Next, we can use the `date.today()` method to get the current date. This method returns a `datetime` object which contains the current date information including the year, month, and day.

```python
today = datetime.date.today()
print(today)
```

The output of this code will vary depending on when you run it, but it will look something like this:

```
2021-10-14
```

We can also format the output to display the date in a specific way using the `strftime()` method. This method allows us to specify a format string to customize the output. For example, if we want to display the date in the format of "Month DD, YYYY", we can use the following code:

```python
today = datetime.date.today()
formatted_date = today.strftime('%B %d, %Y')
print(formatted_date)
```

The output of this code will be:

```
October 14, 2021
```

We can also get information about the current date, such as the day of the week or the day of the year, using the `weekday()` and `strftime()` methods, respectively.

## Deep Dive

Behind the scenes, the `datetime` module is using the system's internal clock to get the current date and time. This means that if your system clock is not accurately set, the current date returned by the `date.today()` method will also be inaccurate.

Additionally, the `datetime` module also allows us to work with timezones and perform various operations on dates, such as adding or subtracting days or comparing dates.

There are also other ways to get the current date using other modules such as `time` or `calendar`. It's worth exploring these options to see which one works best for your particular use case.

## See Also

- [Python datetime documentation](https://docs.python.org/3/library/datetime.html)
- [Python time module documentation](https://docs.python.org/3/library/time.html)
- [Python calendar module documentation](https://docs.python.org/3/library/calendar.html)
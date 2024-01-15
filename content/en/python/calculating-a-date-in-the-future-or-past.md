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

## Why

Calculating dates in the future or past is a common task in many applications, whether it's for scheduling tasks, tracking events, or simply planning ahead. By understanding how to do this in Python, you can easily incorporate this functionality into your code and make your programs more dynamic.

## How To

To calculate a date in the future or past in Python, you can use the ```datetime``` module. First, import the module and then create a ```datetime``` object with the desired date. For example, to get today's date, you can use:

```Python
import datetime
today = datetime.datetime.today()
```

Now, to calculate a date in the future or past, you can use the ```timedelta``` function from the ```datetime``` module. This function takes in arguments that specify the time interval you want to add or subtract from your current date. For example, to calculate a date 5 days from today, you can use:

```Python
future_date = today + datetime.timedelta(days=5)
print(future_date) # Output: 2021-10-26 17:38:20.207444
```

If you want to calculate a date in the past, simply use a negative number for the time interval. For example, to calculate a date 2 weeks before today, you can use:

```Python
past_date = today + datetime.timedelta(weeks=-2)
print(past_date) # Output: 2021-10-11 17:38:20.207444
```

You can also specify other time intervals such as hours, minutes, seconds, and even microseconds in the ```timedelta``` function.

## Deep Dive

The ```datetime``` module not only allows you to calculate dates in the future or past, but it also has many other useful functions for working with dates and times. For example, you can use the ```strftime``` function to format your dates into a specific string format. You can also use the ```date``` and ```time``` classes to work with just the date or time components of a ```datetime``` object.

Additionally, the ```datetime``` module takes into account time zones, daylight saving time, and leap years, making it a reliable tool for handling dates and times in your code.

## See Also

- Official Python documentation for the ```datetime``` module: https://docs.python.org/3/library/datetime.html
- A tutorial on using the ```datetime``` module for working with dates in Python: https://realpython.com/python-datetime/
- A comprehensive guide to date and time manipulation in Python: https://www.geeksforgeeks.org/python-datetime-module-with-examples/
---
title:                "Python recipe: Getting the current date"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Why

One of the most essential functions in any programming language is the ability to work with dates and times. Whether you're creating a calendar application or tracking user activity, having the current date at your disposal is crucial. In this blog post, we'll explore how to get the current date using Python.

# How To

Getting the current date in Python is a simple process that can be achieved using the built-in `datetime` module. In order to access this module, we first need to import it into our code:

```Python
import datetime
```

Once we have imported the `datetime` module, we can use the `datetime` object to get the current date. This object has a method called `today()` which returns the current date as a date object:

```Python
current_date = datetime.datetime.today()
```

We can then print the current date to the console using the `print()` function:

```Python
print("Today's date is: " + str(current_date))
```

The output of this code will be something like this:

```
Today's date is: 2021-08-10 15:30:00.281404
```

If we want to format the output in a specific way, we can use the `strftime()` method. This method takes in a format string as an argument and returns a string representation of the current date according to that format. For example, if we want the date to be displayed as "08/10/21", we can use the format "%m/%d/%y" (month/day/year) as follows:

```Python
current_date = datetime.datetime.today()
formatted_date = current_date.strftime("%m/%d/%y")
print(formatted_date)
```

The output of this code will be:

```
08/10/21
```

You can find a list of all the available format codes for the `strftime()` method in the [Python documentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior).

# Deep Dive

Now that we know how to get the current date using Python, let's take a closer look at the `datetime` module. This module provides many useful functions for working with date and time objects. Let's explore some of the most commonly used ones:

- `today()` - returns the current date and time as a `datetime` object.
- `date()` - returns the date part of a `datetime` object (year, month, day).
- `time()` - returns the time part of a `datetime` object (hour, minute, second, microsecond).
- `weekday()` - returns the day of the week as an integer (Monday is 0 and Sunday is 6).
- `ctime()` - converts a `datetime` object into a string in the format "Day Month dd hh:mm:ss yyyy".
- `strftime()` - formats a `datetime` object according to a specified format string.

For a more in-depth understanding of the `datetime` module and its methods, refer to the [official Python documentation](https://docs.python.org/3/library/datetime.html).

# See Also

- [Python datetime module documentation](https://docs.python.org/3/library/datetime.html)
- [Python strftime() method documentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
- [Real Python article on working with dates and times](https://realpython.com/python-datetime/)

Now that you know how to get the current date in Python, you can start incorporating this functionality into your own projects. Keep exploring the `datetime` module to discover even more ways to work with dates and times in your code. Happy coding!
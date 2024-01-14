---
title:                "Python recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

As we go about our daily lives, we often need to keep track of the current date. But manually checking and updating the date on every device we use can quickly become a tedious and time-consuming task. That's where Python comes in to save the day! With just a few lines of code, we can easily get the current date and automate this process.

## How To

To get the current date in Python, we will be using the built-in `datetime` module. First, we need to import the module into our program:

```Python
import datetime
```

Next, we can use the `datetime.now()` function to get the current date and time. We can store this value in a variable and then use the `date()` method to extract just the date from the datetime object:

```Python
current_date = datetime.now().date()
print(current_date)
```

Running this code will give us the current date in the format `YYYY-MM-DD`. But what if we want to display the date in a different format? We can use the `strftime()` method to format the date according to our preferences. For example, if we want to display the date in the format `DD/MM/YYYY`, we can use the following code:

```Python
current_date = datetime.now().date()
formatted_date = current_date.strftime('%d/%m/%Y')
print(formatted_date)
```

The `strftime()` method takes a formatting string as an argument which specifies how the date should be displayed. You can find a list of formatting options in the official Python documentation.

## Deep Dive

Now that we know how to get the current date in Python, let's take a deeper look at the `datetime` module. This module offers a wide range of functions to manipulate dates and times. For example, we can use the `timedelta` class to perform date calculations. This class allows us to add or subtract a specific number of days, weeks, or even months from a given date. Here's an example of subtracting 7 days from the current date:

```Python
previous_week = datetime.now().date() - datetime.timedelta(days=7)
print(previous_week)
```

We can also compare dates using the comparison operators, such as `<` or `>`. This can be useful when we want to check if a certain date comes before or after another date.

## See Also

- [Python documentation on datetime module](https://docs.python.org/3/library/datetime.html)
- [strftime formatting options](https://strftime.org/)
- [timedelta class documentation](https://docs.python.org/3/library/datetime.html#datetime.timedelta)
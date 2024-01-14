---
title:                "Python recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered the need to display a date in a specific format in your Python program? Converting a date into a string can be a useful skill to have in your programming arsenal. It allows you to customize the presentation of the date and make it more readable for your users.

## How To

Converting a date into a string in Python is a simple process using the `strftime()` function from the `datetime` module. Here's an example of how to use it:

```Python
from datetime import datetime

# Creating a datetime object for today's date
today = datetime.today()

# Converting the date into a string format
date_string = today.strftime('%B %d, %Y')

# Printing the result
print(date_string)
```

This code will output the current date in the format of "Month day, Year", such as "October 08, 2021". Let's break down the code and understand how it works.

First, we import the `datetime` module. This module contains the `datetime` class, which allows us to work with date and time values in Python. Next, we create a `today` object using the `datetime.today()` function, which returns the current date and time.

Then, we use the `strftime()` function to convert the `today` object into a string format. The function takes in a formatting string as its argument, which specifies how the date should be displayed. In this example, we used `%B` for the full month name, `%d` for the day of the month with a leading zero, and `%Y` for the four-digit year.

Finally, we print the `date_string` variable, which now contains the formatted date. You can experiment with different formatting strings and see the output for yourself.

## Deep Dive

The `strftime()` function offers a wide range of formatting options to convert a date into a string. Here are some of the most commonly used formatting codes:

- `%a` - Abbreviated weekday name (Mon, Tue, Wed, etc.)
- `%A` - Full weekday name (Monday, Tuesday, Wednesday, etc.)
- `%b` - Abbreviated month name (Jan, Feb, Mar, etc.)
- `%B` - Full month name (January, February, March, etc.)
- `%d` - Day of the month (01, 02, 03, etc.)
- `%m` - Month as a number (01 for January, 02 for February, etc.)
- `%Y` - Four-digit year (2021)
- `%y` - Two-digit year (21)
- `%H` - Hour in 24-hour format (00 - 23)
- `%I` - Hour in 12-hour format (01 - 12)
- `%p` - AM/PM designation for time
- `%M` - Minute (00 - 59)
- `%S` - Second (00 - 59)

For a full list of available formatting codes, you can refer to the [Python documentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior).

## See Also

- [Converting a String to a Date in Python](https://www.programiz.com/python-programming/datetime/string-datetime)
- [Python datetime module](https://docs.python.org/3/library/datetime.html)
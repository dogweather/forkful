---
title:                "Converting a date into a string"
html_title:           "Python recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string in Python is all about changing the format of a datetime object into a readable string representation. This comes in handy when you want to display a date in a user-friendly way or when you want to manipulate or compare dates more flexibly.

## How To:

To convert a date into a string in Python, you will use the `strftime()` function from the `datetime` module. Here's a simple example:

```Python
from datetime import datetime

# current date and time
now = datetime.now()

# convert the date into a string format
date_string = now.strftime("%m-%d-%Y, %H:%M:%S")

print("Date and Time:", date_string)
```
When you run this code, it gives an output like this:

```
Date and Time: 08-30-2022, 14:45:37
```

## Deep Dive:

The `strftime()` function has been part of Python's `datetime` module since its release in Python 2.3 back in 2002. It's heavily influenced by the C library's 'strftime' function. 

Alternatives to `strftime()` includes:
- The `ctime()` function, which returns a string representing the datetime in the format: 'Sun Sep 16 01:03:52 2022'.
- For Python 3.7 and above, there's `isoformat()`, which returns a string representing a date in ISO 8601 format.

Underneath the hood, `strftime()`'s implementation makes extensive use of the `tm` struct in C. It converts the date elements into a `tm` struct, and then uses format codes to convert this into a string form.

## See Also:

For a complete list of `strftime()` and `strptime()` behavior, refer to Python [official documentation](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior). 

For more guidance with formats, refer to this helpful resource on [Python `strftime()`](http://strftime.org/). 

For a quick comparison between `strftime()`, `ctime()`, and `isoformat()`, refer to this [Stackoverflow discussion](https://stackoverflow.com/questions/466345/converting-date-from-yyyy-mm-dd-to-dd-mm-yyyy).
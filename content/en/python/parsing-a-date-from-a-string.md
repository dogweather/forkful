---
title:                "Parsing a date from a string"
html_title:           "Python recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string involves converting a text string into a date object that your code can understand and manipulate. Programmers do it because it's a quick way to input, read or manage dates within their code.

## How to:

Let's assume you have a date represented as a string, in the format "dd-mm-yyyy". You want to convert this string into a date object in Python. Here's how to do it using the datetime module in Python:

```Python
from datetime import datetime

date_string = "30-04-2021"
date_object = datetime.strptime(date_string, "%d-%m-%Y")

print(date_object)
```

This would output the following:

```Python
2021-04-30 00:00:00
```

## Deep Dive

Historically, different computer languages have provided built-in functions for parsing dates. Python introduced the datetime module in version 2.3. 

An alternative to using datetime could be external libraries like `dateutil` which offers more flexibility, such as recognizing fairly human-readable dates, like "21st of April, 2021".

Implementation-wise, date parsing in Python is quite straightforward. The `strptime` function converts a string to a datetime object, according to a specified format.

However, bear in mind that you need to define the exact format of the date string. If the format is inconsistent or incorrect, `strptime` would throw a ValueError.

## See Also

1. [`datetime` documentation](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)
2. [`dateutil` library](https://dateutil.readthedocs.io/en/stable/)
3. [Python's DateTime](https://realpython.com/python-datetime/) - A Guide by Real Python.
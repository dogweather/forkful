---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Python Date Parsing: Getting More Out Of Strings

## What & Why?

Date parsing is converting a date represented as a string into a Python `datetime` object. This essential task allows programmers to perform operations like date comparison, addition, subtraction, and more.

## How to:

Python, with its `datetime` module, makes date parsing a walk in the park. Here's an example:

```Python
from datetime import datetime

# Date string
date_string = "12-11-2021"
date_format = "%m-%d-%Y"

# Convert string to date
date_object = datetime.strptime(date_string, date_format)

print(date_object)
```
When you run the code, you’ll get:

```Python
2021-12-11 00:00:00
```
The `strptime` function does the job here, taking the string and format, and returning a `datetime` object.

## Deep Dive

Python's `datetime` module, introduced in version 2.3, built on earlier time modules to provide robust utility for manipulating dates and times. 

One alternative to `datetime`'s `strptime` method is the `dateutil.parser.parse` function, which accepts date strings without needing a format, but can be less precise:

```python
from dateutil.parser import parse

# Date string
date_string = "12-11-2021"

# Convert string to date
date_object = parse(date_string)

print(date_object)
```
This will also give you:

```Python
2021-12-11 00:00:00
```

When it comes to the actual implementation, `strptime` works by compiling the format string into a regular expression and matching it against the date string. It then uses the matched parts to create a datetime object.

## See Also:

1. [Python strptime documentation ](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
2. [Full list of Python date directives](https://strftime.org/)
3. [Dateutil documentation](https://dateutil.readthedocs.io/en/stable/parser.html)
4. [Python Regular Expressions](https://docs.python.org/3/library/re.html) for understanding the underlying mechanism.
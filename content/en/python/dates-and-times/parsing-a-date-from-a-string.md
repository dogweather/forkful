---
date: 2024-02-03 19:02:33.694461-07:00
description: "How to: Python's standard library provides the `datetime` module, which\
  \ includes the `strptime` method for this purpose. The method requires two\u2026"
lastmod: '2024-03-13T22:44:59.716491-06:00'
model: gpt-4-0125-preview
summary: Python's standard library provides the `datetime` module, which includes
  the `strptime` method for this purpose.
title: Parsing a date from a string
weight: 30
---

## How to:
Python's standard library provides the `datetime` module, which includes the `strptime` method for this purpose. The method requires two arguments: the date string and a format directive that specifies the pattern of the input string.

```python
from datetime import datetime

# Example string
date_string = "2023-04-01 14:30:00"
# Parsing string to datetime object
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Output: 2023-04-01 14:30:00
```

For more nuanced date parsing, especially when dealing with multiple formats or locales, the third-party library `dateutil` can be extremely helpful. It provides a parser module which can parse dates in almost any string format.

```python
from dateutil import parser

# Example strings
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# Using dateutil's parser
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Output: 2023-04-01 14:30:00
print(parsed_date2)
# Output: 2023-04-01 14:30:00
```

`dateutil` is adept at handling most date formats without explicit format strings, making it a versatile choice for applications dealing with diverse date representations.

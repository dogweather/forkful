---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:37:52.892706-07:00
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means converting text into a date object. We do this because it's easier to manipulate dates, calculate differences, or format them when they're not stuck as plain text.

## How to:
Python's `datetime` module is your go-to for date parsing. Here's a quick guide:

```python
from datetime import datetime

date_string = "2023-04-01"
date_object = datetime.strptime(date_string, "%Y-%m-%d")

print(date_object)  # Output: 2023-04-01 00:00:00

# Wanna see another format? Let's try "day-month-year".
another_date_string = "01-April-2023"
another_date_object = datetime.strptime(another_date_string, "%d-%B-%Y")

print(another_date_object)  # Output: 2023-04-01 00:00:00
```

## Deep Dive
Parsing has been essential since databases and user interfaces started dancing together. Historically, data was often stored as strings, even dates. Now, though, we have the `datetime` module introduced in Python 2.3 (and significantly improved since).

You're not stuck with `datetime`. You could use third-party libraries like `dateutil`, which is more forgiving with formats, or `pandas` for heavy-lifting in data analysis.

Implementation wise, `strptime` stands for "string parse time" and uses format codes to recognize patterns. This means you've got to tell Python the date-string's format, like `%Y` for four-digit year or `%d` for day.

## See Also
- The datetime documentation: https://docs.python.org/3/library/datetime.html
- Dateutil's parser: https://dateutil.readthedocs.io/en/stable/parser.html
- Pandas' to_datetime function: https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.to_datetime.html

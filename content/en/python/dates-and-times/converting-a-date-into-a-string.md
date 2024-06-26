---
changelog:
- 2024-04-04, dogweather, edited
date: 2024-01-20 17:37:21.660960-07:00
description: "How to: Python makes it easy to convert dates to strings. Use the [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-\u2026"
lastmod: 2024-04-04
model: gpt-4-1106-preview
summary: Python makes it easy to convert dates to strings.
title: Converting a date into a string
weight: 28
---

## How to:
Python makes it easy to convert dates to strings. Use the [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) method available on [date](https://docs.python.org/3/library/datetime.html#date-objects) objects. Here's how:

```Python
from datetime import datetime

# Get the current date and time
now = datetime.now()

# Convert it to a string in the format: Month day, Year
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Output: March 29, 2023 (or current date)

# Format: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Output: 2023-03-29 (or current date)
```


### How I do it

This is how I get an [ISO 8601](https://www.w3.org/QA/Tips/iso-date) format date with timezone info:

```python
def datestamp() -> str:
    """ 
    The current date and time with timezone in ISO format.
    """
    return datetime.now().astimezone().isoformat()
```

#### Example output:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Deep Dive
Historically, date-string conversion has been a staple in programming due to the need to represent dates in a human-readable format. 

Alternatives to `strftime` include using the `isoformat` method for ISO 8601 format, or third-party libraries like `arrow` and `dateutil` that offer more flexible parsing and formatting options.

Implementation-wise, `strftime` stands for "string format time" and has roots in C programming. Python's `strftime` interprets format codes like `%Y` for the year and `%m` for the month, allowing for almost endless customizability.

## See Also
To dive deeper into Python's date and time functions:
- Python's official `datetime` documentation: https://docs.python.org/3/library/datetime.html
- For those interested in a comprehensive list of `strftime` directives: https://strftime.org/
- To explore third-party date/time libraries: 
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/

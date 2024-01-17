---
title:                "Getting the current date"
html_title:           "Python recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date is a common task in programming. It simply means finding out what the current date is according to the system. Programmers often need to get the current date in order to perform various tasks such as timestamping, scheduling tasks, or displaying the current date to the user.

## How to:
To get the current date in Python, we can use the built-in `datetime` module. We first import the module using `import datetime`. Then, we can use the `datetime.today()` method to get the current date. Here's an example:

```python
import datetime

current_date = datetime.today()
print(current_date)
```

This will output the current date and time in a `datetime` format, which looks like this: `2021-01-01 12:00:00`. If we want to display the current date in a specific format, we can use the `strftime()` method. Here's an example:

```python
import datetime

current_date = datetime.today()
formatted_date = current_date.strftime('%d/%m/%Y')
print(formatted_date)
```

This will output the current date in the format `01/01/2021`. You can change the format by changing the string provided to the `strftime()` method. The table below shows some common format codes:

| Code | Meaning     |
|------|-------------|
| %d   | Day (01-31) |
| %m   | Month (01-12)|
| %Y   | Year (e.g. 2021) |
| %H   | Hour (00-23) |
| %M   | Minute (00-59) |
| %S   | Second (00-59) |

## Deep Dive:
The `datetime` module was introduced in Python 2.4 as a replacement for the outdated `time` module. It provides more functionality and is easier to use. Another alternative for getting the current date in Python is using the `date` module from the `datetime` package. 

Under the hood, the `datetime` module uses the C language's standard library `libc` to access the system's clock. This means that the accuracy of the current date and time depends on the operating system's clock. In some cases, the system clock can be affected by time drift or synchronization issues, so it's always a good practice to check the system's clock before relying on the current date.

## See Also:
- [Python's datetime module documentation](https://docs.python.org/3/library/datetime.html)
- [Python's strftime() documentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
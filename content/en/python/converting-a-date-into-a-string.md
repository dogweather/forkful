---
title:                "Python recipe: Converting a date into a string"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting dates into strings is a common task in programming, especially when working with databases or APIs. By converting a date into a string, you can easily manipulate and display the date in a desired format. This can be useful for creating user-friendly interfaces or generating reports.

## How To

In Python, there are several ways to convert a date into a string. Let's explore three methods using the built-in datetime module.

* First, we can use the `strftime` method to specify a desired format for the date. This method takes in a format string as its argument, which contains placeholders for different components of the date. For example:

```Python
import datetime

today = datetime.date.today()

# Convert date into string with format YYYY-MMM-DD
formatted_date = today.strftime("%Y-%b-%d")

print(formatted_date) # Output: 2021-Aug-10
```

* Second, we can use the `isoformat` method to convert a date into ISO 8601 format. This is a standardized date format that is commonly used for data exchange. For example:

```Python
import datetime

today = datetime.date.today()

# Convert date into ISO 8601 format
iso_date = today.isoformat()

print(iso_date) # Output: 2021-08-10
```

* Lastly, we can also use the `str` function to convert a date object into a string. This method does not provide any formatting options, but it returns a string representation of the date in a default format. For example:

```Python
import datetime

today = datetime.date.today()

# Convert date into string
str_date = str(today)

print(str_date) # Output: 2021-08-10
```

## Deep Dive

When converting a date into a string, it's important to understand the different format codes that can be used in the `strftime` method. These codes represent different components of a date, such as year, month, and day. Some common format codes include:

* `%Y` - Year as a four-digit number
* `%m` - Month as a zero-padded decimal number
* `%d` - Day of the month as a zero-padded decimal number
* `%b` - Month as abbreviated name
* `%B` - Month as full name
* `%w` - Weekday as a decimal number (0-6, with Sunday being 0)
* `%Z` - Time zone name

For a full list of format codes, you can refer to the Python documentation for `strftime`.

## See Also

* [Python datetime module documentation](https://docs.python.org/3/library/datetime.html)
* [ISO 8601 format guide](https://www.iso.org/iso-8601-date-and-time-format.html)
* [strftime format codes reference](https://strftime.org/)
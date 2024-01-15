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

## Why

So, you want to know how to get the current date in Python? Well, the current date is essential for many applications and can be used for tasks such as creating time-sensitive data, scheduling events, or simply displaying the date to the user.

## How To

```Python
from datetime import date

# This method returns the current date in year-month-day format
current_date = date.today()
print(current_date)
```

Output:
```
2021-08-19
```

The `date` module from the `datetime` library allows us to work with dates in Python. We can use the `today()` method to get the current date and assign it to a variable. Then, by calling the `print()` function, we can display the current date in a specific format.

If you want to display the current date in a different format, you can use the `strftime()` method. This method takes in a format string as an argument and returns the date in the specified format.

```Python
from datetime import date

# This method returns the current date in day-month-year format
current_date = date.today()
formatted_date = current_date.strftime("%d-%m-%Y")
print(formatted_date)
```

Output:
```
19-08-2021
```

Some common format codes for the `strftime()` method are:

- `%Y` - year in four digits
- `%m` - month in two digits
- `%d` - day in two digits
- `%B` - full month name
- `%b` - abbreviated month name
- `%A` - full weekday name
- `%a` - abbreviated weekday name

You can also use the `datetime` object to get the current time along with the date. It has the following attributes that can be accessed using the dot notation:

- `year`
- `month`
- `day`
- `hour`
- `minute`
- `second`

```Python
from datetime import datetime

# This method returns the current date and time
current_date_time = datetime.now()

# Accessing the hour and minute attributes
hour = current_date_time.hour # 24-hour format
minute = current_date_time.minute # 0-59 range

print(f"The current time is {hour}:{minute}")
```

Output:
```
The current time is 15:45
```

## Deep Dive

Now, let's take a deeper look at how the `date` and `datetime` modules work in Python.

The `date` object has the following attributes:

- `year` - four-digit number
- `month` - integer ranging from 1 (January) to 12 (December)
- `day` - integer ranging from 1 to 31 (depending on the month)

These attributes can be accessed individually using the dot notation. For example: `current_date.year`

The `datetime` object is similar to the `date` object, but it also includes the time attributes such as `hour`, `minute`, and `second`. Additionally, the `datetime` object has the `microsecond` attribute, which represents the current microsecond of the time.

To get the current date and time, we use the `now()` method. It returns the current date and time as a `datetime` object.

```Python
from datetime import datetime

# This method returns the current date and time
current_date_time = datetime.now()
print(current_date_time)
```

Output:
```
2021-08-19 15:50:18.429279
```

## See Also

- [Python datetime module documentation](https://docs.python.org/3/library/datetime.html)
- [strftime() format codes](https://strftime.org/)
- [Python date and time tutorial](https://www.programiz.com/python-programming/datetime)
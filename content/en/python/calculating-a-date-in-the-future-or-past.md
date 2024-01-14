---
title:                "Python recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why 

If you ever need to schedule an event or have a project deadline, it can be helpful to know how to calculate a date in the future or past. This skill can also be useful in everyday life, such as planning vacations or keeping track of important dates.

## How To 

Calculating a date in the future or past is made easy with the `datetime` module in Python. First, you'll need to import the module using `import datetime`. Then, you can use the `datetime` class to create a new date object. Here's an example of getting the date one month from now:

```Python
import datetime

# Get current date
today = datetime.date.today()

# Add one month
one_month = datetime.timedelta(days=30)

# Calculate date one month from now
future_date = today + one_month

# Print result in mm/dd/yyyy format
print(future_date.strftime("%m/%d/%Y"))
```
**Output: 08/26/2021**

Similarly, you can also calculate a date in the past by using a negative number of days in the `timedelta` function. Here's an example of getting the date five years ago:

```Python
import datetime

# Get current date
today = datetime.date.today()

# Subtract five years
five_years = datetime.timedelta(days=-1826)

# Calculate date five years ago
past_date = today + five_years

# Print result in yyyy-mm-dd format
print(past_date.strftime("%Y-%m-%d"))
```
**Output: 2016-08-14**

## Deep Dive 

The `timedelta` function used in the examples above is a powerful tool for calculating dates in the future or past. You can specify the number of days, weeks, months, or even years to add or subtract from a date. Additionally, the `strftime()` function allows you to format the date output in any way you want.

It's also worth noting that the `datetime` module can handle leap years and other calendar complexities, making it a reliable way to calculate dates.

## See Also 

For more information about the `datetime` module and its capabilities, you can check out the official documentation: 

- [Python datetime module documentation](https://docs.python.org/3/library/datetime.html)
- [Python timedelta object documentation](https://docs.python.org/3/library/datetime.html#timedelta-objects)
- [Python strftime documentation](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)

Start incorporating date calculations into your code and see how it can make your life easier!
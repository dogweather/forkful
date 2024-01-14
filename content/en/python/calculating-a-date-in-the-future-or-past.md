---
title:    "Python recipe: Calculating a date in the future or past"
keywords: ["Python"]
---

{{< edit_this_page >}}

##Why
Have you ever found yourself needing to calculate a date in the future or past? Whether it's for travel planning, project deadlines, or simply curiosity, being able to accurately calculate dates can be a useful skill for any programmer.

##How To
Calculating a date in the future or past is simple with Python's built-in `datetime` module. Here's an example of how to calculate a future date:

```Python
from datetime import date, timedelta

# today's date
today = date.today()
# 30 days from today
future_date = today + timedelta(days=30)

# print the future date in a formatted string
print("In 30 days, it will be:", future_date.strftime("%m/%d/%Y"))
```

The output of this code would be: `In 30 days, it will be: 08/18/2021`.

Similarly, if you wanted to calculate a past date, you can use a negative number of days in the `timedelta` function. Here's an example:

```Python
from datetime import date, timedelta

# today's date
today = date.today()
# 2 weeks ago
past_date = today - timedelta(weeks=2)

# print the past date in a formatted string
print("2 weeks ago was:", past_date.strftime("%m/%d/%Y"))
```

This would output `2 weeks ago was: 07/28/2021`.

##Deep Dive
The `datetime` module also allows for more complex date calculations, such as finding the difference between two dates or formatting dates in different ways. You can also use the `dateutil` module to handle more advanced date and time operations.

For example, to find the number of days between two dates, you can use the `date` function and subtract them:

```Python
from datetime import date

# two arbitrary dates
date1 = date(2021, 8, 15)
date2 = date(2021, 8, 1)

# find the difference between the dates
difference = date1 - date2

# print the number of days
print("The difference is", difference.days, "days.")
```

This would output `The difference is 14 days.`

For more advanced date and time operations, the `dateutil` module is a great resource. You can find more information about it in the [official documentation](https://dateutil.readthedocs.io/en/stable/).

##See Also
- [Python `datetime` module documentation](https://docs.python.org/3/library/datetime.html)
- [Date and time operations with the `dateutil` module](https://dateutil.readthedocs.io/en/stable/)
- [Calculating future and past dates in Python](https://able.bio/rhett/calculating-future-and-past-dates-in-python--57c5f3c8)
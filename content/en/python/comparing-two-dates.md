---
title:                "Comparing two dates"
html_title:           "Python recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates in programming refers to the process of determining if one date is earlier or later than the other. This is important for tasks such as sorting and filtering data based on date values. Programmers often need to compare dates in order to accurately analyze and manipulate data in their code.

## How to:
To compare two dates in Python, first import the datetime module which allows for date and time operations. Then, use the `datetime` function to create two date objects. Finally, use comparison operators, such as `<` (less than) and `>` (greater than), to compare the two dates. 

```
# Import datetime module
import datetime

# Create date objects
date1 = datetime.date(2021, 5, 1)
date2 = datetime.date(2021, 5, 15)

# Compare dates
if date1 < date2:
    print("Date 1 is earlier than Date 2")
elif date1 > date2:
    print("Date 1 is later than Date 2")
else:
    print("The two dates are equal")
```

The output in this case would be: `Date 1 is earlier than Date 2`

## Deep Dive:
In a historical context, comparing dates was much more complex in older programming languages before libraries like the datetime module were created. Programmers had to use a combination of functions and operators to compare dates, often resulting in lengthier and less efficient code.

An alternative to using the datetime module in Python is to use the `timedelta` function, which allows for date and time calculations. However, this may not be necessary for simple date comparisons.

Under the hood, the comparison operators in Python work by comparing the number of days between the two dates as integers. This is why we can use these operators with date objects.

## See Also:
For more information on comparing dates in Python, check out the official documentation for the datetime module: https://docs.python.org/3/library/datetime.html

For a more in-depth tutorial on working with dates in Python, you can also refer to this guide: https://realpython.com/python-datetime/
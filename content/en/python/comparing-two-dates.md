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

Comparing two dates is about determining if one date is earlier, the same, or later than another date. Programmers do this to arrange events chronologically or evaluate time periods.

## How to:

Here's how you can compare two dates using Python's `datetime` module:

```Python
from datetime import date

# Define two dates
date1 = date(2021, 12, 15)
date2 = date(2021, 12, 20)

# Compare the dates
if date1 < date2:
     print("date1 is earlier")
elif date1 == date2:
     print("The dates are the same")
else:
     print("date1 is later")
```

The output will be:
```
date1 is earlier
```

## Deep Dive

Python's `datetime` module came to life in 2003 as a part of Python 2.3, allowing programmers to deal more efficiently with dates and times. 

An alternative to `datetime` can be third-party libraries, like `Pendulum` or `Arrow`, which offer extended functionality and ease of use.

In Python, when you compare dates, it's actually comparing tuples of the presented date in the format (year, month, day). If you were to write `date1 < date2`, Python internally translates this to `(2021, 12, 15) < (2021, 12, 20)` and evaluates the comparison accordingly.

## See Also

Check out these other useful resources for more info:

- Python's official [datetime documentation](https://docs.python.org/3/library/datetime.html)
- `Pendulum` library [documentation](https://pendulum.eustace.io/docs/)
- `Arrow` library [documentation](https://arrow.readthedocs.io/en/latest/)
- A helpful [StackOverflow thread](https://stackoverflow.com/questions/15307623/can-python-compare-date-string-data-to-see-which-is-more-recent) about date comparison in Python.
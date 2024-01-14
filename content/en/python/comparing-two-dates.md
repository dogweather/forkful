---
title:                "Python recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to compare two dates in your Python program? Maybe you're working on a project that requires checking the difference between two dates, or maybe you simply want to know which date comes first. Whatever the reason may be, comparing dates is a common task in programming and can be easily accomplished in Python. In this blog post, we will explore how to compare two dates in Python and dive into the underlying concepts.

## How To

To compare two dates in Python, we will be using the `date` class from the `datetime` module. First, let's import the necessary module:

```Python
import datetime
```

Next, we need to create two date objects with the `date()` constructor. Here's an example:

```Python
date1 = datetime.date(2021, 7, 10)
date2 = datetime.date(2021, 7, 15)
```

Now that we have our two dates, we can use the `>` and `<` operators to compare them. The `>` operator checks if the left operand is greater than the right, while the `<` operator checks if the left operand is less than the right. Using these operators, we can easily determine which date comes first. Let's see some examples:

```Python
# compare two dates
date1 > date2  # False
date1 < date2  # True

# compare same dates
date1 > date1  # False
date2 < date2  # False

# compare dates with different years
date1 > datetime.date(2020, 7, 10)  # True
```

As you can see, we can compare dates just like we compare numbers. This makes it very easy to check the relationship between two dates. However, what if we want to check if two dates are exactly the same? For that, we can use the `==` operator, just like we do with other data types.

```Python
date1 == date2  # False
date1 == datetime.date(2021, 7, 10)  # True
```

## Deep Dive

Behind the scenes, Python uses the `__lt__()`, `__gt__()`, and `__eq__()` magic methods to perform the comparison operations. These methods are used to define the behavior of an object when compared with another object. In our case, the `date` class has already defined these methods, which is why we can easily compare dates using the operators. However, if you are creating your own class, you will need to define these methods to enable comparison.

It's also important to note that the `date` objects are immutable, meaning they cannot be changed once created. This is why we use the `date()` constructor to create new date objects, rather than changing the existing ones.

## See Also

* [Python Documentation on Date Objects](https://docs.python.org/3/library/datetime.html#date-objects)
* [Real Python Blog on Working with Dates and Times in Python](https://realpython.com/python-datetime/)
* [Python Tutorial on Comparing Objects](https://www.programiz.com/python-programming/comparison-operators#object)
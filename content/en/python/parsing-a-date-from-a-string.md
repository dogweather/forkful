---
title:                "Parsing a date from a string"
html_title:           "Python recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string in Python refers to extracting the date information (month, day, year) from a string of text. This is commonly done by programmers to convert user input or data from different sources into a standardized date format that can be used for calculations or comparisons.

## How to:
Parsing a date from a string in Python is made easy with the built-in datetime module. Here's a simple example:
```Python
from datetime import datetime

date_string = "01/12/2021"
date_object = datetime.strptime(date_string, "%m/%d/%Y")
print(date_object.strftime("%Y-%m-%d"))
```
Output:
```
2021-01-12
```
The ```strptime()``` function takes in the date string and a format string that specifies the order of the date components. The ```strftime()``` function then formats the date object into the desired output format.

## Deep Dive:
Parsing dates from strings has been a common task for programmers for a long time, especially in languages like Python which have a large number of date and time formats. Before the datetime module was introduced, programmers had to manually split the string and convert the date components into integers.

An alternative to the datetime module is the dateutil library, which provides a parser that can handle a wider range of date formats. However, this library is not included in the standard Python installation and requires additional installation.

The implementation of the datetime module for parsing dates uses a combination of the ```strptime()``` and ```strftime()``` functions, which are based on the C language's ```strptime()``` and ```strftime()``` functions.

## See Also:
To learn more about parsing dates in Python, check out the official documentation for the datetime module: https://docs.python.org/3/library/datetime.html

For more information on the dateutil library, visit their official website: https://dateutil.readthedocs.io/en/stable/

To explore different date and time formats, take a look at the strftime reference: https://strftime.org
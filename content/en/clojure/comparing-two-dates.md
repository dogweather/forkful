---
title:                "Comparing two dates"
html_title:           "Clojure recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates is the process of determining whether one date is earlier, later, or equal to another date. This is a common task for programmers when working with date-related data, such as scheduling events or calculating durations. It allows them to efficiently organize and manipulate data based on their chronological order.

## How to:
To compare two dates in Clojure, we can use the `compare` function. This function takes two date values and returns an integer that indicates the relationship between the two dates. Here's an example:

```Clojure
(compare (java.util.Date. 2000 1 1) (java.util.Date. 2000 1 2))
```

This will return -1, indicating that the first date is earlier than the second date. Here are all the possible return values and their corresponding relationships:

- -1: First date is earlier than second date
- 0: Dates are equal
- 1: First date is later than second date 

## Deep Dive:
The `compare` function in Clojure is based on the `java.util.Date` class, which represents a specific moment in time. This class has a `compareTo` method that is used by the `compare` function to determine the relationship between two dates.

An alternative way to compare dates in Clojure is to use the `before?` and `after?` functions. These functions take two date values and return a boolean value indicating whether the first date is before or after the second date.

Implementing date comparison in Clojure is relatively straightforward due to its built-in support for the `java.util.Date` class. However, it's important to be aware of potential issues with time zones and daylight saving time when dealing with date comparisons.

## See Also:
- [Clojure Date and Time library](https://github.com/clj-time/clj-time)
- [Official Clojure Documentation on java.util.Date](https://clojure.org/reference/java_interop#_java_util_date)
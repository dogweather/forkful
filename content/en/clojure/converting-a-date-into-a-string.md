---
title:    "Clojure recipe: Converting a date into a string"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a common task in programming, especially when working with dates in different formats. In Clojure, this task can be easily accomplished using built-in functions.

## How To

To convert a date into a string, you can use the `str` and `format` functions in Clojure. The `str` function converts any data type into a string, while the `format` function allows you to specify the desired format for the date.

Let's take a look at an example of converting the current date into a string:

```Clojure
(str (format (java.util.Date.) "yyyy-MM-dd"))
```

This code will return a string in the format of "year-month-day", for example "2020-05-12".

If you want to include the time in the string, you can use the `HH:mm:ss` format. Here's an example:

```Clojure
(str (format (java.util.Date.) "yyyy-MM-dd HH:mm:ss"))
```

This will return a string in the format of "year-month-day hour:minute:second", for example "2020-05-12 23:45:10".

## Deep Dive

When working with dates in Clojure, it's important to keep in mind the different types of data structures that can represent a date. The `java.util.Date` class represents a specific moment in time, while the `java.util.Calendar` class represents a date with more precision, including milliseconds.

Additionally, Clojure also has its own data structures for representing dates, such as `#inst` and `#date`. These data structures are useful when performing date calculations and comparisons.

It's also worth noting that Clojure supports date formatting using the Joda-Time library. This library offers advanced formatting options, such as specifying locale and time zones.

## See Also

- [Clojure Docs on date and time functions](https://clojuredocs.org/clojure.core/str)
- [Joda-Time library for Clojure](https://github.com/clj-time/clj-time)
- [Java Docs for java.util.Date class](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java Docs for java.util.Calendar class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
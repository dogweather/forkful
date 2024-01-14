---
title:                "Clojure recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to know the current date in a Clojure program? Whether it's for tracking user activity, scheduling tasks, or simply displaying the date to the user, there are many reasons why you might want to get the current date in your code.

## How To
To get the current date in Clojure, we can use the built-in `java.time.LocalDate` class. This class provides methods for working with dates and times and is part of the standard Java library, making it easily accessible in Clojure.

To start, we need to import the `java.time` package in our namespace:
```Clojure
(ns my-namespace
  (:import (java.time LocalDate)))
```

Next, we can create a new `LocalDate` object by calling the `now` method:
```Clojure
(def current-date (LocalDate/now))
```

We can then use the `format` method to customize the output of our date, using the `java.time.format.DateTimeFormatter` class. For example, if we want to display the date in the format of "MM/dd/yyyy", we can do so as follows:
```Clojure
(def formatted-date (.format current-date (DateTimeFormatter/ofPattern "MM/dd/yyyy")))
```

The `formatted-date` variable will now hold a string representation of the current date. We can also access specific components of the date, such as the month, day, and year:
```Clojure
(def current-month (.getMonth current-date))
(def current-day (.getDayOfMonth current-date))
(def current-year (.getYear current-date))
```

When we run our code, we should see the current date and values for the individual components printed out. For example, if today's date is January 1st, 2021, the output would be:
```
01/01/2021
1
1
2021
```

## Deep Dive
Behind the scenes, the `LocalDate` class uses the system clock to get the current date. It also takes into account the time zone and daylight savings to ensure accuracy.

One interesting feature of the `java.time` package is that it provides support for other calendar systems, not just the traditional Gregorian calendar. This allows users to work with dates and times in different cultural contexts.

If you need to work with times as well as dates, you can also use the `java.time.LocalTime` class in a similar manner.

## See Also
For more information on the `java.time` classes and how to work with dates and times in Clojure, check out the following resources:
- [Official Java documentation for the `java.time` package](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Clojure documentation on using Java interop](https://clojure.org/reference/java_interop)
- [Comprehensive guide to dates and times in Clojure](https://databyteacademy.medium.com/the-comprehensive-guide-to-dates-and-times-in-clojure-7724095f2975)
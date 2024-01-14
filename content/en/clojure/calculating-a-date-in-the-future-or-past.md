---
title:    "Clojure recipe: Calculating a date in the future or past"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past may seem like a simple task, but it can actually be quite complex. Whether you're trying to schedule events or track deadlines, having the ability to accurately calculate dates is essential for any programming task. In this blog post, we'll explore how to do this using Clojure.

## How To

To calculate a date in the future or past, we'll be using the `clj-time` library. This library provides functions for working with dates and times in Clojure. First, we need to import the library into our program:

```Clojure
(ns my-program.core
  (:require [clj-time.core :as t]))
```

Next, we can use the `plus` and `minus` functions to add or subtract days, months, or years to a given date. For example, to calculate a date 2 weeks from today:

```Clojure
(def today (t/today))
(t/plus today (t/weeks 2))
```

This will return the date object for 2 weeks in the future. Similarly, to calculate a date 10 days in the past:

```Clojure
(def today (t/today))
(t/minus today (t/days 10))
```

The `plus` and `minus` functions also accept multiple arguments, allowing us to add or subtract different units of time. For example, to calculate a date 3 years and 2 months in the future:

```Clojure
(def today (t/today))
(t/plus today (t/years 3) (t/months 2))
```

## Deep Dive

Under the hood, the `plus` and `minus` functions are using the Joda-Time library to perform the calculations. Joda-Time is a popular Java library for working with dates and times, and `clj-time` provides a Clojure wrapper for it. This allows us to take advantage of all the functionality Joda-Time has to offer.

One particularly useful function in Joda-Time is `withDayOfWeek`, which allows us to find a date that falls on a specific day of the week. For example, if we wanted to find the date for the next Monday after a given date, we can use the following code:

```Clojure
(def today (t/today))
(t/plus today (t/withDayOfWeek 1))
```

This will return the next Monday's date. We can also pass in a negative number to find a date in the past. For example, to find the previous Friday's date:

```Clojure
(def today (t/today))
(t/minus today (t/withDayOfWeek -5))
```

## See Also

- Joda-Time library: https://www.joda.org/joda-time/
- Clojure Time library documentation: https://clj-time.github.io/clj-time/
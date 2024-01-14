---
title:                "Clojure recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is a commonly encountered task in programming. It can be used for tasks such as scheduling events, tracking time-based data, or displaying countdowns. In Clojure, there are built-in functions that make date calculations easy and efficient.

## How To
Calculating dates in Clojure can be done using the `clj-time` library, which provides powerful date/time manipulation functions. First, we need to import the `clj-time.core` library into our code:

```Clojure
(ns my-project.core
  (:require [clj-time.core :as t]))
```

Next, we can use the `t/plus` and `t/minus` functions to add or subtract a specified time interval from a given date. For example, if we want to calculate the date that is 7 days in the future, we can use the following code:

```Clojure
(def today (t/today))
(t/plus today (t/days 7))
```

This will return a `DateTime` object representing the date 7 days from today. Similarly, if we want to calculate a date in the past, we can use the `t/minus` function. For example, to calculate the date 2 weeks ago, we can use the following code:

```Clojure
(def today (t/today))
(t/minus today (t/weeks 2))
```

The `clj-time` library also provides other useful functions for date manipulation, such as `t/interval`, `t/in-days`, and `t/in-months`. Refer to the library's documentation for more details.

## Deep Dive
The `clj-time` library is built on top of the popular Java library Joda-Time, which has been recognized for its intuitive and efficient API for date/time calculations. This means that Clojure developers can benefit from the robustness and reliability of Joda-Time while enjoying the simplicity and elegance of Clojure syntax.

Behind the scenes, `clj-time` uses the concept of "duration" to calculate dates in the future or past. A duration is a period of time represented by a number of milliseconds. By using the `t/days`, `t/weeks`, `t/months`, and other duration functions, we can easily create a duration object that specifies the desired time interval. Then, we can use the `t/plus` and `t/minus` functions to add or subtract that duration from a given date.

## See Also
- [Official Clojure Documentation](https://clojure.org/)
- [clj-time Github Repository](https://github.com/clj-time/clj-time)
- [Joda-Time Documentation](https://www.joda.org/joda-time/)
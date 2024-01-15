---
title:                "Calculating a date in the future or past"
html_title:           "Clojure recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many applications, whether it's for scheduling events, setting reminders, or determining deadlines. Clojure offers a powerful library for working with dates and times, making it easy to perform these calculations accurately and efficiently.

## How To
To calculate a date in the future or past in Clojure, we will be using the `clj-time` library. First, we need to add the library as a dependency in our project's `project.clj` file:

```Clojure
[clj-time "0.15.1"]
```

Next, we can use the `plus` and `minus` functions from the `clj-time.core` namespace to add or subtract a specific amount of time from a given date. Let's see some examples:

```Clojure
(ns my-project.date-calculator
  (:require [clj-time.core :as t]))

;; Calculate 10 days from today
(t/plus (t/today) (t/days 10)) ;=> #object[org.joda.time.DateTime 0x5489e344 "2019-12-26T00:00:00.000Z"]

;; Calculate 2 weeks from a specific date
(t/plus (t/date-time 2019 12 20) (t/weeks 2)) ;=> #object[org.joda.time.DateTime 0x2aedf924 "2020-01-03T00:00:00.000Z"]

;; Calculate 6 months ago from now
(t/minus (t/now) (t/months 6)) ;=> #object[org.joda.time.DateTime 0x41e19f8f "2019-06-25T22:01:55.000Z"]
```

As we can see, the `clj-time` library makes it easy to perform date calculations by providing functions to represent different units of time, such as days, weeks, months, etc.

## Deep Dive
Behind the scenes, the `clj-time` library uses the Joda-Time library to handle date and time calculations. This makes `clj-time` a powerful and reliable tool for working with dates in Clojure.

It is important to note that the `plus` and `minus` functions return a `DateTime` object, which is a data structure from the Joda-Time library. This object contains all the necessary information about the date and time, such as the year, month, day, hour, minute, second, and time zone.

Additionally, the `clj-time` library provides functions to extract specific information from a `DateTime` object, such as `t/year`, `t/month`, `t/day-of-month`, `t/hour`, `t/minute`, `t/second`, and `t/time-zone`.

## See Also
- [clj-time documentation](https://github.com/clj-time/clj-time)
- [Joda-Time documentation](https://www.joda.org/joda-time/)
- [Clojure cheat sheet](https://clojure.org/api/cheatsheet)
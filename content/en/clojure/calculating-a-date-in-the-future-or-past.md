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

## What & Why?

Calculating a date in the future or past refers to the process of determining a specific date that is a certain amount of time away from a given starting date. This can be useful for tasks such as scheduling events or calculating deadlines. Programmers often need to do this in their code in order to automate these types of calculations and make their programs more efficient.

## How to:

To calculate a date in the future or past using Clojure, you can use the `plus` and `minus` functions from the `clj-time` library. These functions take in a date and a duration, and return a new date that is the result of either adding or subtracting the duration from the initial date. Here is an example of how to use them:

```Clojure
(require '[clj-time.core :as time])

(time/minus (time/today) (time/days 7)) ;; subtracts 7 days from today's date
;; => #inst "2021-06-30T00:00:00.000-00:00"

(time/plus (time/today) (time/months 3)) ;; adds 3 months to today's date
;; => #inst "2021-09-30T00:00:00.000-00:00"
```

You can also use the `days` and `months` functions from the `clj-time.core` namespace to specify the duration in days or months. The resulting dates will be returned as Clojure instants, which are a representation of a specific point in time.

## Deep Dive:

In the past, calculating dates in a programming language was a complex task that required a lot of code and tedious calculations. With the `clj-time` library, this process is now much easier and more efficient. Other alternatives for calculating dates in Clojure include using the Java 8 time API or creating custom functions to do the calculations manually.

Under the hood, the `plus` and `minus` functions use the `clj-time.period` namespace to handle durations. This library handles the complexity of different month lengths, leap years, and daylight saving time, making it a reliable option for date calculations.

## See Also:

- [clj-time documentation](https://github.com/clj-time/clj-time)
- [Java time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
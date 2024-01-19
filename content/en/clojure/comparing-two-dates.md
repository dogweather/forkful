---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparing Two Dates in Clojure 

## What & Why?

Comparing two dates involves determining if one date is earlier, later, or the same as another date. Programmers do this to implement functionalities like sorting events, calculating time intervals, and setting time-based constraints.

## How to:

Clojure has built-in functions for date comparison. Below is an example of how to compare two dates using `compare` function.

```Clojure
(import 'java.time.LocalDate)

(def date1 (LocalDate/of 2022 3 15))
(def date2 (LocalDate/of 2023 3 15))

(defn compare-dates [date1 date2]
  (compare date1 date2))

(println (compare-dates date1 date2))
```

The compare function returns `-1` if the first date is earlier, `1` if the first date is later, and `0` if both dates are the same. In the above example, it will output `-1`.

## Deep Dive

Historically, date comparison in Clojure could get messy due to the quirks of `java.util.Date`. Thankfully, `java.time.LocalDate` in JDK 8 onwards simplifies date handling, and Clojure, being a JVM language, can take full benefit of it.

You can use `before` or `after` methods of `LocalDate` instance for more specific comparisons:

```Clojure
(.isAfter date1 date2)
(.isBefore date1 date2)
```
These will return either `true` or `false`.

For non-JDK means, you can rely on libraries like `clj-time` which provide a Clojure-friendly interface for Joda Time, a rich date-time library.

## See Also

1. Clojure docs on [Date-Time](https://clojure.org/about/date_time): Offers insights into Clojure's date and time handling capabilities.
2. Official Java 8 [LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html) docs
3. [clj-time](https://github.com/clj-time/clj-time) on GitHub: A Clojure Library for creating, parsing and manipulating dates and times.
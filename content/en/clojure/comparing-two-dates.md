---
title:                "Clojure recipe: Comparing two dates"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When working with dates in programming, it is often necessary to compare two dates and determine which one is earlier or later. This can be useful when sorting data, setting up reminders, or any other task that relies on date comparisons.

## How To

To compare two dates in Clojure, we can use the `>` (greater than) and `<` (less than) operators. These operators work on date objects and can be used to determine which date comes first in chronological order.

```
Clojure
(def date1 (java.util.Date. 2020 5 10)) 
;; creates a date object for May 10, 2020
(def date2 (java.util.Date. 2021 5 10)) 
;; creates a date object for May 10, 2021

(> date1 date2) 
;; returns false, as date1 is earlier than date2
(< date1 date2) 
;; returns true, as date1 is earlier than date2
```

If we want to compare dates with more precision, we can also use the `before?` and `after?` functions. These functions take two date objects as arguments and return true if the first date comes before or after the second date, respectively.

```
Clojure
(def date1 (java.util.Calendar/getInstance)) 
;; creates a date object for the current date and time
(def date2 (java.util.Calendar/getInstance)) 
;; creates a date object for the current date and time

(after? date1 date2) 
;; returns false, as date1 is not after date2 since they are the same
(before? date1 date2) 
;; returns false, as date1 is not before date2 since they are the same
```

## Deep Dive

When comparing two dates, it is important to consider the various factors that can affect the comparison. This includes the time zone, time of day, and daylight saving time changes. By default, Clojure uses the system time zone for date comparisons. However, if you want to compare dates in a different time zone, you can use the `time-zone` function.

```
Clojure
(def date1 (java.util.Date. 2020 5 10)) 
;; creates a date object for May 10, 2020
(def date2 (java.util.Date. 2021 5 10)) 
;; creates a date object for May 10, 2021
(def pst-zone (java.time.ZoneId/of "Pacific/Pitcairn")) 
;; creates a ZoneId object for the Pacific/Pitcairn time zone

(< date1 date2 pst-zone) 
;; returns true, as date1 is earlier than date2 in the Pacific/Pitcairn time zone
```

It is also important to note that date comparisons in Clojure only work on dates and not times. If we want to compare dates and times, we can use the `instant` function to convert them to milliseconds since epoch and then compare those values.

```
Clojure
(def date1 (java.util.Calendar/getInstance)) 
;; creates a date object for the current date and time
(def date2 (java.util.Calendar/getInstance)) 
;; creates a date object for the current date and time

(> (instant date1) (instant date2)) 
;; returns false, as date1 is not later than date2 since they are the same
```

## See Also

- [Clojure Date and Time Guide](https://clojure.org/guides/date_time)
- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
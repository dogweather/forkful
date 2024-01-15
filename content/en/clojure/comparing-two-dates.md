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

## Why

Comparing dates is a common task in many programming applications, allowing us to check for events that occurred before or after a certain date or to calculate the difference between two dates. In Clojure, there are a few ways to compare dates and determine their relative positions, making it a useful skill for any developer to have.

## How To

To compare two dates in Clojure, we first need to understand the data types used for representing dates: `java.util.Date` and the Clojure-specific `#inst` tag. The `java.util.Date` class represents a specific moment in time, while the `#inst` tag represents an instant on the timeline.

### Comparing `java.util.Date` objects

To compare `java.util.Date` objects, we can use the `compare` function, which returns -1 if the first date is earlier, 0 if they are equal, and 1 if the first date is later. This function takes two date objects as arguments, like so:

```Clojure
(def date1 (java.util.Date. 2020 5 10))
(def date2 (java.util.Date. 2020 5 11))

(compare date1 date2)
;; Output: -1
(compare date2 date1)
;; Output: 1
(compare date1 date1)
;; Output: 0
```

### Comparing `#inst` tags

To compare `#inst` tags, we can use the `before?` and `after?` functions, which return true or false depending on their relative positions. These functions also take two arguments, like `compare`.

```Clojure
(def date1 #inst "2020-05-10T00:00:00.000-00:00")
(def date2 #inst "2020-05-11T00:00:00.000-00:00")

(before? date1 date2)
;; Output: true
(after? date1 date2)
;; Output: false
(before? date1 date1)
;; Output: false
```

### Calculating the difference between dates

To determine the difference between two dates, we can use the `days` function, which returns the number of days between two `java.util.Date` objects.

```Clojure
(def date1 (java.util.Date. 2020 5 10))
(def date2 (java.util.Date. 2020 5 14))

(days date2 date1)
;; Output: 4
```

## Deep Dive

While the methods above allow us to compare dates easily, they may not give us the most accurate results in all situations. For example, daylight saving time and leap years can affect the comparison outcome. To handle these scenarios, we can use the `com.clojure/java-time` library, which provides a more robust and accurate date/time API.

This library uses the `java.time` package from Java 8, which offers a wide range of functionality for working with dates and times. It also supports time zones and daylight saving time adjustments.

## See Also

- Java-time library: https://github.com/dm3/clojure.java-time
- ClojureDocs  `java.util.Date` documentation: https://clojuredocs.org/clojure.core/date
- ClojureDocs `#inst` tag documentation: https://clojuredocs.org/clojure.core/inst
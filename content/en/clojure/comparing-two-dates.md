---
title:    "Clojure recipe: Comparing two dates"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Why
In programming, we often need to compare two dates to determine which one comes before or after the other. This is an essential skill for applications that deal with scheduling, events, and any time-based data.

# How To
Comparing two dates in Clojure is straightforward and can be done using various built-in functions and libraries. Let's take a look at a few examples:

### Comparing using `>`
```Clojure
(def today (java.util.Date.)) ; create a date object for today
(def tomorrow (java.util.Date. (inc (.getYear today)) (.getMonth today) (.getDate today))) ; create a date object for tomorrow

(> tomorrow today) ; returns true
(> today tomorrow) ; returns false
```

### Comparing using `compare`
```Clojure
(def date1 (java.util.Date. 2021 4 15)) ; create a date object for 15th April 2021
(def date2 (java.util.Date. 2021 4 20)) ; create a date object for 20th April 2021

(compare date1 date2) ; returns -1 as date1 comes before date2
(compare date2 date1) ; returns 1 as date2 comes after date1
(compare date1 date1) ; returns 0 as both dates are equal
```

### Comparing using `clj-time`
Clojure also has a useful library called `clj-time` for working with time and dates. Let's see an example:

```Clojure
(require '[clj-time.core :refer [after before after? before?]])
(def date1 (clj-time.core/now)) ; current date
(def date2 (clj-time.core/plus-date date1 1 :days)) ; add 1 day to date1
(def date3 (clj-time.core/plus-date date1 2 :days)) ; add 2 days to date1

(after? date2 date1) ; returns true as date2 is after date1
(before? date3 date1) ; returns false as date3 is not before date1
```

# Deep Dive
When comparing two dates, it is essential to understand that Clojure uses the Java `Date` class, which represents a specific point in time. This means that comparing two dates only compares the exact time values and not just the dates. So if you have two dates that are different by only a few milliseconds, they will not be considered equal.

Additionally, the `Date` class is mutable, meaning that the values of the dates can change. This can lead to unexpected behavior if you are not careful. It is best to always work with immutable date objects, especially when dealing with comparisons.

# See Also
- [Java Date class documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Date.html)
- [Clojure `clj-time` library](https://github.com/clj-time/clj-time)
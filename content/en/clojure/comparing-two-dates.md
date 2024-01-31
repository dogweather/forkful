---
title:                "Comparing two dates"
date:                  2024-01-20T17:32:28.116760-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparing two dates"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates means checking how they relate—is one earlier, later, or the exact same as the other? Programmers do this to handle deadlines, schedule events, and track time-related data.

## How to:
Clojure uses the Java interop capabilities to handle dates. Let's roll up our sleeves and dive in:

```clojure
;; Import Java Date class
(import java.util.Date)

;; Create two date instances
(def date1 (java.util.Date.))
(Thread/sleep 1000) ;; Wait a bit
(def date2 (java.util.Date.))

;; Compare the dates
(println (.before date1 date2)) ; true, date1 is before date2
(println (.after date1 date2))  ; false, date1 is not after date2
(println (.equals date1 date2)) ; false, date1 is not the same as date2
```

Sample output might look like this, but with different time stamps:

```
true
false
false
```

## Deep Dive
In the past, Clojure developers often used Java's `Date` for date operations, invoking methods using the dot operator as seen earlier. Alternatives include `clj-time`, a Clojure library wrapping Joda-Time.

An example using `clj-time` would look like this:

```clojure
;; Add clj-time to your project dependencies
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; Create two date-time instances
(def date-time1 (time/now))
(Thread/sleep 1000) ;; Wait a second
(def date-time2 (time/now))

;; Compare using clj-time functions
(println (time/before? date-time1 date-time2)) ; true
(println (time/after? date-time1 date-time2))  ; false
(println (time/equal? date-time1 date-time2))  ; false
```

Clojure's stance on time is leveraging Java's libraries, while clj-time integrates with Joda-Time for a more idiomatic Clojure experience.

Since Java 8, the `java.time` package—inspired by Joda-Time—is the preferred way to deal with dates and times in Java and, by extension, in Clojure through interop. Improved design and additional capabilities like time zones make `java.time` a robust choice.

## See Also
- [Clojure's Java Interop](https://clojure.org/reference/java_interop)
- [clj-time GitHub Repository](https://github.com/clj-time/clj-time)
- [Java Date and Time API Guide](https://docs.oracle.com/javase/tutorial/datetime/)

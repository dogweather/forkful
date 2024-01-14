---
title:                "Clojure recipe: Getting the current date"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, getting the current date is a common task when building applications. Knowing the date and time is essential for a variety of functions such as scheduling tasks, logging events, and displaying information. In this blog post, we will explore how to get the current date using Clojure.

## How To

In Clojure, getting the current date is straightforward and can be done in a few different ways. Let's take a look at some examples using the `java.time` library.

First, we can use the `now` function to get the current date and time. It returns a `java.time.Instant` object, which represents a moment on the timeline in UTC time zone.

```Clojure
(require '[java.time :as t])
(t/now)
;; #inst "2021-07-29T09:29:55.064172100Z"
```

We can also use the `local-date` function to get the current date in our local time zone. It returns a `java.time.LocalDate` object, which represents a date without a time component.

```Clojure
(require '[java.time :as t])
(t/local-date)
;; #object[java.time.LocalDate 0x114a53a "2021-07-29"]
```

To get the current time in our local time zone, we can use the `local-time` function, which returns a `java.time.LocalTime` object representing just the time component.

```Clojure
(require '[java.time :as t])
(t/local-time)
;; #object[java.time.LocalTime 0x3c48cb58 "09:30:52.710456700"]
```

We can also get the current date and time together as a `java.time.LocalDateTime` object using the `local-date-time` function.

```Clojure
(require '[java.time :as t])
(t/local-date-time)
;; #object[java.time.LocalDateTime 0x336be680 "2021-07-29T09:32:19.003277400"]
```

Lastly, we can get the current date and time in a specific time zone using the `at-zone` function. This returns a `java.time.ZonedDateTime` object with the date and time in the desired time zone.

```Clojure
(require '[java.time :as t])
(t/at-zone (t/now) (t/zone-id "America/New_York"))
;; #object[java.time.ZonedDateTime 0x1007a2cb "2021-07-29T05:33:09.386729500-04:00[America/New_York]"]
```

## Deep Dive

In the above examples, we used the `java.time` library, which is part of the Java standard library. This library introduced a new approach to working with date and time in Java, inspired by the Joda-Time library.

The `java.time` library is designed to be easy to use and understand while also providing a rich set of functions for working with date and time. It follows the principle of immutability and returns new objects when modifying existing ones.

It is important to note that the `now` function and `local-date-time` function use the system's default time zone. However, the `local-date`, `local-time`, and `at-zone` functions allow us to specify a desired time zone.

## See Also

- Official ClojureDocs for java.time library: https://clojuredocs.org/clojure.java-time
- Clojure Cheatsheet for Date and Time: https://clojure.org/api/cheatsheet#date_and_time
- Joda-Time library: https://www.joda.org/joda-time/
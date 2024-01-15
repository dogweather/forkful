---
title:                "Getting the current date"
html_title:           "Clojure recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why 

Getting the current date is a common task in programming, especially in applications that require time-sensitive information or operations. Fortunately, in Clojure, there are simple and efficient ways to retrieve the current date.

## How To 

To retrieve the current date in Clojure, we can use the `clojure.java-time` library, which provides a set of functions for working with dates and times.

First, we need to require the library in our namespace:

```Clojure 
(ns my-project.core
  (:require [clojure.java-time :as t]))
```

Then, we can use the `now` function to get the current date and time in milliseconds since the epoch:

```Clojure
(t/now)
;=> #object[java.time.Instant 0x100d0c7a "2021-10-26T14:30:10.349Z"]
```

We can also specify a specific time zone by using the `now-zoned` function, which takes a parameter for the time zone identifier as defined by the `java.time.ZoneId` class:

```Clojure
(t/now-zoned "Asia/Tokyo")
;=> #object[java.time.ZonedDateTime 0x1ece360 "2021-10-27T00:30:10.349+09:00[Asia/Tokyo]"]
```

To get the current date in a more readable format, we can use the `today` function, which returns a `java.time.LocalDate` object representing the current date in the default time zone:

```Clojure
(t/today)
;=> #object[java.time.LocalDate 0x1b7c97c6 "2021-10-26"]
```

We can also format the date using the `formatter` function, which takes a `java.time.format.DateTimeFormatter` object as a parameter:

```Clojure
(t/formatter (t/today))
;=> "Oct 26, 2021"
```

## Deep Dive 

Under the hood, Clojure's date and time functions use the `jsr-310` standard, which is built into the Java runtime environment. This makes it easy to work with date and time data without having to import external libraries.

Additionally, the use of immutable data structures in Clojure ensures that the retrieved dates are not modified or affected by any changes to the system time or time zone.

## See Also

- [Clojure.java-time Documentation](https://cljdoc.org/d/clojure.java-time/clojure.java-time/0.3.2/doc/readme)
- [Java SE 8 Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [JSR-310: Date and Time API](https://jcp.org/en/jsr/detail?id=310)
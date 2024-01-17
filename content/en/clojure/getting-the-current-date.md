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

## What & Why?

Getting the current date is a common task in programming, especially when building applications that require time-based functionality. It allows us to display the current date and time to users, track when events occur, and perform calculations based on dates and times. It is important for creating dynamic and relevant user experiences.

## How to:

To get the current date in Clojure, we can use the `java.util.Date` class and the `java.util.Calendar` class. First, we will import the necessary libraries:

```Clojure
(require '[java.util.Date :as date])
(require '[java.util.Calendar :as cal])
```

Next, we can use the `Date` class to create a new date object representing the current date:

```Clojure
(def current-date (date.))
```

To get more information about the current date, we can use the `Calendar` class to manipulate the date object:

```Clojure
(def calendar (cal/getInstance))
(.setTime calendar current-date)
```

We can then retrieve specific information such as the year, month, and day:

```clojure
(.get calendar cal/YEAR)
;; => 2021

(.get calendar cal/MONTH)
;; => 6 (Note: Clojure uses zero-based indexing, so January is represented as 0)

(.get calendar cal/DAY_OF_MONTH)
;; => 29
```

We can also use the `Date` class to get the current time:

```Clojure
(def current-time (.getTime current-date))
```

And to get the current time in milliseconds:

```Clojure
(def current-time-millis (.getTimeInMillis calendar))
```

Finally, we can use the `printf` function to format the current date and time in a specific way:

```Clojure
(printf "Current Date: %tc" current-time)
;; => Current Date: Tue Jun 29 16:40:32 EDT 2021
```

## Deep Dive:

Clojure is a modern functional programming language that runs on the Java Virtual Machine (JVM). As a result, it can easily access Java libraries and classes, making it straightforward to get the current date. However, it is not the only way to get the current date in Clojure. Another option is to use the `clj-time` library, which provides more advanced date and time manipulation functions.

In terms of implementation, the `Date` class in Clojure is a wrapper for the `java.util.Date` class in Java. It represents a specific point in time, with millisecond precision. The `Calendar` class is also a wrapper for the `java.util.Calendar` class in Java, which allows for manipulation and retrieval of specific date and time information.

## See Also:

- [Clojure API Documentation](https://clojure.github.io/clojure/)
- [clj-time library](https://github.com/clj-time/clj-time)
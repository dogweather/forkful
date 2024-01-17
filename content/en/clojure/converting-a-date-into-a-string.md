---
title:                "Converting a date into a string"
html_title:           "Clojure recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string means turning a date object into a human-readable string format. Programmers do this for various reasons such as displaying dates on user interfaces, logging events, or writing data into a file.

## How to:
To convert a date into a string in Clojure, we can use the `format` function from the `java.time.format` library. Below is an example of converting today's date into a string in the format of DD/MM/YYYY:

```Clojure
(require '[java.time.format :as format])
(format/DateTimeFormatter/ofPattern "DD/MM/YYYY" (java.time.LocalDate/now))
```

The output of this code would be `"16/02/2021"`, where `16` is the current day, `02` is the current month, and `2021` is the current year.

## Deep Dive:
Historically, converting dates into strings has been a challenging task due to differences in date formats across different countries and cultures. However, Java introduced the `java.time` API in version 8, making it easier to work with dates and times in Clojure. Alternatively, programmers can use the `clojure.java-time` library, which provides additional functionalities for working with dates in Clojure.

## See Also:
- [Official Clojure Doc on Dates and Times](https://clojure.org/reference/java_interop#_dates_and_times)
- [Clojure.java-time Library](https://github.com/pesterhazy/java-time)
- [Clojure For The Brave And True - Dates and Times](https://www.braveclojure.com/dates-and-times/)
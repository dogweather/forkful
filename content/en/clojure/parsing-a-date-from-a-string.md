---
title:                "Parsing a date from a string"
html_title:           "Clojure recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of converting a date that is represented as a string into a more structured data type that can be used in a program. This is commonly done when dealing with user input or when working with date-based data. This allows programmers to manipulate and compare dates more easily and accurately within their code.

## How to:

To parse a date from a string in Clojure, we can use the ```clojure.core/date-time``` function. This function takes in a string representing a date and returns a data-time object.

```clojure
(clojure.core/date-time "2020-06-12")
```

The output of this code would be:

```clojure
#object[java.time.LocalDateTime 0x34f42314 "2020-06-12T00:00:00"]
```

This data-time object can then be used in various operations and comparisons within our program.

## Deep Dive:

Parsing dates has been a long-standing challenge in programming, as date formats can vary greatly and can be difficult to convert into a standardized format. In Clojure, the ```clojure.core/date-time``` function uses the ISO-8601 format to convert a string into a data-time object. However, there are other alternative libraries such as ```clojure.java-time``` that offer more robust options for handling dates and times in Clojure, including support for different date formats and time zones.

The implementation of the ```clojure.core/date-time``` function relies on the Java ```java.time.LocalDateTime``` class, which is part of the Java 8 Date and Time API. This provides a more modern and comprehensive approach to working with dates and times in Java and Clojure.

## See Also:

- [Clojure Documentation for date-time function](https://clojuredocs.org/clojure.core/date-time)
- [Clojure Java Time Library](https://github.com/dm3/clojure.java-time)
- [Java 8 Date and Time API Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
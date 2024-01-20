---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date in programming means simply retrieving the system's present date. This function is used frequently when you need to timestamp when a particular event has occurred or to compare with other dates for scheduling purposes.

## How to:
Kotlin makes it simple to get the current date using the `LocalDate` class from the java.time package. Let's go straight to coding.

```Kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()

    println("The current date is: $currentDate")
}
```
The code sample is pretty straightforward. We import the `LocalDate` class and then use the `now()` function. Once you run this script, it should print the current date in a yyyy-MM-dd format. For instance:

```
The current date is: 2022-09-29
```

## Deep Dive
Historically, programmers were limited to using the traditional `java.util.Date` class in Java which had quite a few issues, including being not thread-safe and having convoluted APIs. With the advent of Java 8, the date-time APIs were completely revamped and became part of the java.time package, which Kotlin also effectively uses.

As an alternative, you can still use the traditional `java.util.Date` class if you wish to.

```Kotlin
import java.util.Date

fun main() {
    val currentDate = Date()

    println("The current date is: $currentDate")
}
```

This will output the current date in this format: Wed Sep 29 14:45:41 GMT 2022. 

In terms of implementation details, the `LocalDate.now()` function internally uses a system clock to obtain the current date. It gets a clock reading of the current instant and then converts it into the date using the time-zone.

## See Also
For a more extensive understanding of date/time handling in Kotlin, you can refer to the official Kotlin documentation:
- [Kotlin Date and Time](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)

For date-time best practices and Java 8 date-time APIs (which Kotlin uses):
- [Oracle Docs – Date-Time](https://docs.oracle.com/javase/tutorial/datetime/iso/overview.html) 
- [Java 8 Date Time API](https://www.journaldev.com/2800/java-8-date-localdate-localdatetime-instant)
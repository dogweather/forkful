---
title:                "Getting the current date"
html_title:           "Kotlin recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why 

If you're a programmer, chances are you'll need to work with dates and times at some point. Whether you're developing an app, a website, or any other software, getting the current date is a common task that you'll likely encounter. Knowing how to do it in an efficient and precise way can save you time and headaches down the line.

## How To

Here's how to get the current date in Kotlin:

```Kotlin
import java.time.LocalDate

// Using LocalDate.now() method
val currentDate = LocalDate.now()

// Printing the current date
println("Today's date is $currentDate")

// Output: Today's date is 2021-01-01
```

Alternatively, you can also use the `java.util.Date` class and its `getTime()` method to get the current date in milliseconds:

```Kotlin
import java.util.Date

// Creating a Date object using the current time
val currentDate = Date(System.currentTimeMillis())

// Printing the current date
println("Today's date is $currentDate")

// Output: Today's date is Fri Jan 01 00:00:00 GMT 2021
```

## Deep Dive

In Java, the `java.util.Date` class was widely used for handling dates and time. However, with the release of Java 8, the new `java.time` package was introduced, providing more modern and efficient ways of working with dates and times.

In Kotlin, we can also take advantage of these improvements by using the `LocalDate` class from the `java.time` package. It represents a date without a time zone, and the `now()` method returns the current date based on the system clock.

## See Also

- [Java 8 Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/)

By using the methods provided by the `java.time` package, we can effectively and accurately handle dates and times in our Kotlin programs. Take some time to explore and experiment with different methods and classes within the `java.time` package to see what other functionalities are available.
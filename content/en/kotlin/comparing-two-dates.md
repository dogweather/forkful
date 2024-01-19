---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Kotlin 101: Comparing Dates
---
## What & Why?
Simply put, comparing two dates means determining whether one date is earlier, same, or later than another. As programmers, we often need this to trigger actions based on time events, like sending feedback when a trial period ends. 

## How to:
Comparing dates in Kotlin is super straightforward. Use the `isBefore`, `isAfter` or `isEqual` methods. Let's view some code:

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2022, 1, 1)
    val date2 = LocalDate.of(2022, 2, 1)
  
    if (date1.isAfter(date2)) {
        println("$date1 comes after $date2")
    } else if (date1.isBefore(date2)) {
        println("$date1 comes before $date2")
    } else {
        println("The dates are equal")
    }
}
```
Output:
```
2022-01-01 comes before 2022-02-01
```
## Deep Dive:
Historically, date and time handling was quite tedious. With Java 8, the `java.time` package (JSR-310), a comprehensive and 'leap-year proof' framework, was introduced. Kotlin provides native support for this JSR-310 API. 

As alternatives, you could also use `java.util.Date` or `java.util.Calendar` but these are tedious and error-prone. `java.time.LocalDate` is preferred for date comparisons.

The `isBefore`, `isAfter`, and `isEqual` methods use the `compareTo` method under the hood. The `compareTo` method returns a negative integer, zero, or a positive integer if this `LocalDate` is less than, equal to, or greater than the specified `LocalDate`.

## See Also:
To explore more about the Date and Time API, see these docs:

1. [Java SE 8 Date and Time](https://docs.oracle.com/javase/tutorial/datetime/index.html)

2. [Kotlin's java.time.LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/java.time/-local-date/index.html)

Note: This article assumes familiarity with basic Kotlin syntax. If not, start with the [Kotlin docs](https://kotlinlang.org/docs/home.html).
---
date: 2024-01-20 17:31:20.478869-07:00
description: "Calculating a date in the future or past means finding a specific date\
  \ before or after a known one. Programmers do this for features like reminders,\u2026"
lastmod: '2024-03-13T22:45:00.061211-06:00'
model: gpt-4-1106-preview
summary: "Calculating a date in the future or past means finding a specific date before\
  \ or after a known one. Programmers do this for features like reminders,\u2026"
title: Calculating a date in the future or past
weight: 26
---

## What & Why?

Calculating a date in the future or past means finding a specific date before or after a known one. Programmers do this for features like reminders, expiry notifications, or planning tools—anything that's time-sensitive.

## How to:

Kotlin handles dates and times with the `java.time` library. To add or subtract days, use `plusDays()` or `minusDays()`. Here's the skinny:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val tenDaysLater = today.plusDays(10)
    val tenDaysBefore = today.minusDays(10)
    
    println("Today: $today")
    println("Ten days from now: $tenDaysLater")
    println("Ten days ago: $tenDaysBefore")
}
```

Sample output:

```
Today: 2023-03-15
Ten days from now: 2023-03-25
Ten days ago: 2023-03-05
```

Beyond days, you can also play with months and years (`plusMonths()`, `minusMonths()`, `plusYears()`, `minusYears()`).

## Deep Dive

Calculating dates isn't new. Since Java 8, the `java.time` package has been the go-to for date-time arithmetic—much better than old `Calendar` or `Date`, which were clunky and not thread-safe. 

`java.time` uses immutable objects, so you avoid nasty bugs from accidentally modifying your dates. Objects like `LocalDate`, `LocalTime`, `LocalDateTime`, and `ZonedDateTime` help you represent different aspects of time precisely.

Alternatives? Of course. Before `java.time`, Joda-Time was the weapon of choice. Some older systems still use it. And in the Android realm, the ThreeTenABP library backports `java.time` features for compatibility with Java 6 & 7 circumstances.

The `java.time` API is also designed to be timezone-aware, thanks to classes like `ZonedDateTime`. So when you're shimmying dates around, you can respect the chronology of Earth's spin.

## See Also

- The official `java.time` documentation: [Java SE Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- For the Android devs, `ThreeTenABP` library details: [ThreeTenABP on GitHub](https://github.com/JakeWharton/ThreeTenABP)
- An in-depth guide, if you want more nuts-and-bolts on date and time: [Date and Time in Java](https://www.baeldung.com/java-8-date-time-intro)

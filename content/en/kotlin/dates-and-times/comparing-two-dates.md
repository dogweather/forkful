---
date: 2024-01-20 17:33:11.947785-07:00
description: "How to: Historically, Java provided `Date` and `Calendar` classes but\
  \ they weren't very user-friendly. Kotlin uses similar classes under the hood but\u2026"
lastmod: '2024-04-05T22:50:48.616053-06:00'
model: gpt-4-1106-preview
summary: Historically, Java provided `Date` and `Calendar` classes but they weren't
  very user-friendly.
title: Comparing two dates
weight: 27
---

## How to:
```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 10)
    val date2 = LocalDate.of(2023, 5, 15)

    println(date1.isBefore(date2))  // true
    println(date1.isAfter(date2))   // false
    println(date1.isEqual(date2))   // false

    // Comparing using compareTo
    println(date1.compareTo(date2)) // -1 if date1 is before date2
}
```

Sample output:

```
true
false
false
-1
```

## Deep Dive
Historically, Java provided `Date` and `Calendar` classes but they weren't very user-friendly. Kotlin uses similar classes under the hood but encourages using the `java.time` package introduced in Java 8 for better clarity and utility.

There are alternatives like `Instant` for timestamps, `ZonedDateTime` for time-zone specific dates, or using a third-party library like Joda-Time. Keep implementation details in mind—`Instant` uses a traditional Unix timestamp while `LocalDate` abstracts this away and deals with a conceptual day without time or timezone.

Knowing which class best suits your needs is essential. `LocalDate` is fine for most date comparisons, but for precise instant-in-time comparisons, consider `ZonedDateTime` or `Instant`.

## See Also
- The official Kotlin documentation on dates and times: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)
- Java 8 Date and Time guide: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- Joda-Time library: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)

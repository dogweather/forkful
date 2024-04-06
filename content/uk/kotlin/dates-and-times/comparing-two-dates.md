---
date: 2024-01-20 17:33:24.356597-07:00
description: "How to (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438): Output varies depending on today's date."
lastmod: '2024-04-05T21:53:49.435628-06:00'
model: gpt-4-1106-preview
summary: Output varies depending on today's date.
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## How to (Як це зробити):
```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 1)
    val date2 = LocalDate.now()

    println(date1.isBefore(date2)) // true if date1 is before date2
    println(date1.isAfter(date2))  // true if date1 is after date2
    println(date1.isEqual(date2))  // true if date1 is equal to date2
}
```
Output varies depending on today's date.

## Deep Dive (Поглиблений Аналіз):
Kotlin, like Java, has rich support for date-time operations using `java.time` package. Historically, Java's old `Date` and `Calendar` classes were troublesome. Kotlin, benefiting from Java's evolution, recommends `java.time` (JSR-310) for modern date-time handling.

Alternatives? You could use `Date` and `Calendar` from old Java times, but that’s masochism. Stick to `java.time`.

Implementation details? `LocalDate` is just for dates, no times. If you need to compare time, you'd use `LocalDateTime` or `Instant`. `ZonedDateTime` is there for handling time zones.

## See Also (Дивіться також):
- [Oracle's java.time tutorial](https://docs.oracle.com/javase/tutorial/datetime/)
- [Kotlin's Date and Time - Baeldung](https://www.baeldung.com/kotlin/dates)

---
title:                "Порівняння двох дат"
aliases: - /uk/kotlin/comparing-two-dates.md
date:                  2024-01-20T17:33:24.356597-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Comparing two dates means figuring out their order—earlier, later, or the same moment. Programmers compare dates to track events, expire sessions, or sort timelines.

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

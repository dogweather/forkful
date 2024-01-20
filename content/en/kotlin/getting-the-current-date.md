---
title:                "Getting the current date"
date:                  2024-01-20T15:15:24.183018-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
We get the current date to know today's data. It's crucial for tons of features – think logs, trials, events. You name it, dates are often right there.

## How to:
```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Today's date is: $today")
}
```

Sample output:
```
Today's date is: 2023-04-05
```

## Deep Dive
Historically, dates have been a soup of problems for developers. Time zones, leap years, daylights saving; they're tricky. Kotlin banks on `java.time` APIs from Java 8 onwards, making date operations more bearable.

`LocalDate.now()` is our go-to for current dates. No time, no zone – just the date. Need time? There's `LocalTime`. Both? `LocalDateTime`. And if time zones matter, use `ZonedDateTime`.

Alternatives? Before Java 8, `java.util.Date` and `Calendar` ruled. Not great, not terrible, but now somewhat old-fashioned and, frankly, less intuitive.

Under the hood, `LocalDate.now()` snags the system clock. But it's not just any clock – it's the UTC clock, adjusted to your system's default time zone. You can mess with it, sure – pass a different `Clock` or `ZoneId` if you like living on the edge.

## See Also
Kotlin docs on dates and times: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)

Java 8 Date/Time overview: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)

Want to go full historian? Check out the evolution of java.time: [https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html](https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)
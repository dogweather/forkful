---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:37:15.208264-07:00
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date means converting a date in text format into a date object a program can understand and manipulate. It's crucial for reading data from various sources like user input or files, allowing programs to process and handle dates and times consistently.

## How to:
With Kotlin, you can parse dates using the `LocalDateTime` class from the `java.time` package. Let's parse a string into a date.

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-01T15:30:00"
    val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    val parsedDate = LocalDateTime.parse(dateString, formatter)
    
    println(parsedDate)  // Sample Output: 2023-04-01T15:30
}
```

## Deep Dive
Kotlin doesn't have its own date and time library. Instead, it relies on the `java.time` API introduced in Java 8, which replaced older, less intuitive date classes like `java.util.Date`. 

A big plus of `java.time` is that it brought immutability and thread-safety to date-time operations. Before Java 8, you'd often turn to third-party libraries like Joda-Time for robust date handling. 

When parsing dates, you must match the date string to the correct format. Otherwise, you'll face a `DateTimeParseException`. Kotlin's approach is derived from the ISO 8601 standard, but you can create custom formats with `DateTimeFormatter` for different string patterns.

Alternatives to `LocalDateTime` include `ZonedDateTime` for timezone support or `LocalDate` and `LocalTime` for parsing dates and times separately. Kotlin's flexibility with the `java.time` API ensures you can tailor your date parsing to the program's needs.

## See Also
- The official Java `DateTimeFormatter` documentation: [https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Kotlin Documentation on Java Interoperability: [https://kotlinlang.org/docs/java-interop.html](https://kotlinlang.org/docs/java-interop.html)
- ISO 8601 Date and Time Formats: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)

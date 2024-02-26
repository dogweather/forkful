---
date: 2024-02-03 19:02:39.265922-07:00
description: "Parsing a date from a string involves converting text into a Date object.\
  \ This operation is fundamental for applications that interact with dates entered\u2026"
lastmod: '2024-02-25T18:49:56.502949-07:00'
model: gpt-4-0125-preview
summary: "Parsing a date from a string involves converting text into a Date object.\
  \ This operation is fundamental for applications that interact with dates entered\u2026"
title: Parsing a date from a string
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string involves converting text into a Date object. This operation is fundamental for applications that interact with dates entered by users or sourced from external datasets, allowing for easy manipulation and formatting according to needs.

## How to:
Kotlin supports date parsing through the `java.time` package, introduced in Java 8. Here's a simple approach using `LocalDateTime` and a specific pattern:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // Output: 2023-04-01T12:00
}
```

For more flexibility, or to handle dates from external sources like APIs, you might use a third-party library such as Joda-Time (though it's less common now with `java.time` being robust). However, sticking with the modern approach provided by the JDK is preferred for most Kotlin applications.

To parse a date in Kotlin without using third-party libraries, you can also make use of the `SimpleDateFormat` class for versions before Java 8 or Android API levels lacking `java.time` support:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // Output will vary based on your timezone, e.g., Sat Apr 01 12:00:00 GMT 2023
}
```

Remember to always set the timezone if working with `SimpleDateFormat` to avoid unexpected offsets in parsed dates.

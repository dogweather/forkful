---
title:                "Calculating a date in the future or past"
html_title:           "Kotlin recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past is all about shifting dates forward or backward in time. Programmers do it for a variety of reasons, typically related to scheduling, tracking, or predicting events. 

## How to

Kotlin, with its robust standard library, makes this a breeze. Let's see how:

```Kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val today = LocalDate.now()

    // calculcate a date 5 days in the future
    val futureDate = today.plus(5, ChronoUnit.DAYS)
    println("Future date: $futureDate")

    // calculate a date 3 months in the past
    val pastDate = today.minus(3, ChronoUnit.MONTHS)
    println("Past date: $pastDate")
}
```

Running this, you'll get output along the lines:

```Kotlin
Future date: YYYY-MM-DD
Past date: YYYY-MM-DD
```

The exact dates will depend on the current date when you run the code.

## Deep Dive

Historically, programmers had to manually calculate date offsets using cumbersome routines, considering leap years, time zones, etc. Fortunately, modern languages like Kotlin offer in-built classes and methods to make these calculations simple.

There are alternatives. For instance, the Joda-Time library in Java provides more comprehensive date/time manipulation capabilities. However, using Kotlin's standard library suffices for most needs and compromises no efficiency.

Implementation-wise, Kotlin's date/time classes like `LocalDate` employ the widely-adopted ISO-8601 calendar system. They make use of `ChronoUnit`, an enumeration representing temporal units such as 'Days', 'Months', 'Hours', etc., to perform date/time arithmetic.

## See Also

For more details, you might like to explore:

1. Kotlin's date/time classes in standard library: [Kotlin Date & Time](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)
2. An overview on using Joda-Time with Java: [Joda Time library](https://www.joda.org/joda-time/)
3. ISO-8601 calendar system: [ISO 8601 - Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Converting a Date to String in Kotlin

## What & Why?
Converting a date to a string essentially involves the transformation of a date object into a textual format. This is useful for displaying dates in a human-readable format or for serialization purposes.

## How to: 
Kotlin makes transforming dates to strings pretty straightforward - simply use the format function in DateTimeFormatter.

Here's an example:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val current = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formatted = current.format(formatter)

    println("Current Date and Time is: $formatted")
}
```

In this example, LocalDateTime.now() provides the current date and time. You can provide any pattern to the `ofPattern` function of `DateTimeFormatter`. The `format` function is used to format the date.

This will output something like:

```bash
Current Date and Time is: 2022-10-25 09:10:30
```

## Deep Dive

Formatting a time or date is a common task in programming. It dates back to the days of COBOL and punch card programming when systems relied on specific string patterns to interpret date and time. The standard way to convert a date object into a string representation in Kotlin is by using the format method of `DateTimeFormatter`. 

While `DateTimeFormatter` is a go-to choice, there are alternatives like `SimpleDateFormat`. However, it's not recommended in modern Kotlin programming because it's not thread-safe and has performance issues in some cases.

When a date is converted into a string, the time-zone context might be lost if it is not incorporated into the pattern string. This is because the resulting string will represent the date and time relevant to the system's set time-zone.

## See Also

For further information, consider checking out the following resources:

- DateTimeFormatter Official Docs: [Java Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/format/DateTimeFormatter.html)
- An Introduction to Dates and Times in Kotlin: [Baeldung Tutorial](https://www.baeldung.com/kotlin/dates)
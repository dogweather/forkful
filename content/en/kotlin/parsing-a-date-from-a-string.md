---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is simply extracting meaningful date-time information from text. Programmers do this often to use date-time information from user inputs or data files in their programs.

## How to:

In Kotlin, we use `parse()` function from `SimpleDateFormat` class to parse a date from a string. Here is an example:

```Kotlin 
import java.text.SimpleDateFormat
import java.util.Locale

fun main() {
    val dateString = "12/03/2022"
    val format = SimpleDateFormat("dd/MM/yyyy", Locale.ENGLISH)
    val date = format.parse(dateString)
    
    println(date)
}
```

Running this will output:

```Kotlin
Sat Mar 12 00:00:00 PST 2022
```

It reads a date string in `dd/MM/yyyy` format and converts it into a `Date` object.

## Deep Dive

The concept of parsing dates from strings is as old as programming languages. It's prevalent when dealing with real-world date data. Other languages like Python, Java, and JavaScript also have similar functionalities, but approaches and syntaxes may vary.

Instead of `SimpleDateFormat`, one could use `DateTimeFormatter` from `java.time` package in modern Kotlin:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

fun main() {
    val dateString = "12-03-2022"
    val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy", Locale.ENGLISH)
    val date = LocalDate.parse(dateString, formatter)

    println(date)
}
```

This code does the same as above but uses a hyphen `-` as a date separator.

## See Also

- For advanced date-time operations, check out the Joda-Time library: [Joda-Time](https://www.joda.org/joda-time/)
- Kotlin's Date and Time APIs: [Kotlin Date and Time](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)
- A comprehensive guide to date-time operations in Kotlin: [Baeldung Kotlin DateTime](https://www.baeldung.com/kotlin/dates)
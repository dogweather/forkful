---
title:                "Converting a date into a string"
html_title:           "Kotlin recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string means representing a specific date in a textual format that can be easily read and understood by humans. Programmers do this to display dates in a user-friendly manner and to allow for easy manipulation and storage of date data in their code.

## How to:
```Kotlin
// Import the required library
import java.time.LocalDate

// Creating a LocalDate object representing today's date
val currentDate = LocalDate.now()

// Converting the date into a string using the ISO format
val stringDate = currentDate.toString()

// Printing the result
println(stringDate)

// Output: "2021-10-11"
```

## Deep Dive:
Converting dates into strings has been a common problem in computer programming. In the past, dates were represented in various formats such as numbers, letters, or symbols, making it difficult for different systems to communicate with each other. With the advent of standardized date formats, such as ISO 8601, converting dates into strings has become easier and more efficient. In addition, there are alternative ways to convert dates, such as using libraries or custom formatting functions. The implementation of date-to-string conversion in Kotlin utilizes the concept of extension functions, allowing for a simplified and intuitive approach.

## See Also:
- [Kotlin documentation on LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date)
- [ISO 8601 standard for date and time representation](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Different date format alternatives in Java](https://docs.oracle.com/javase/tutorial/datetime/iso/format.html)
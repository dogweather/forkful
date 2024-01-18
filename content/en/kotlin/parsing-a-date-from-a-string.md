---
title:                "Parsing a date from a string"
html_title:           "Kotlin recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string refers to the process of extracting a date from a given string input. This is a common task in programming as it allows developers to manipulate and work with dates in various formats. By parsing a date from a string, programmers can perform operations such as formatting, validation, and conversion to different time zones.

## How to:

### Simple Date Parsing:

To parse a date from a string in Kotlin, we can use the built-in `DateFormat` class. Here's an example:

```Kotlin
val dateString = "01/02/2020"             // String representing a date
val format = SimpleDateFormat("MM/dd/yyyy")   // Specify the format of the date
val date = format.parse(dateString)      // Parse the date from the string
println(date)                           // Output: Fri Jan 02 00:00:00 EST 2020
```

### Custom Date Parsing:

Sometimes, the date format is not a standard one and might require some customization. In such cases, we can use the `DateTimeFormatter` class to define our own parsing patterns. Here's an example:

```Kotlin
val dateString = "Wednesday, June 24, 2020"   // String representing a date
val format = DateTimeFormatter.ofPattern("EEEE, MMMM dd, yyyy") // Specify the custom format
val date = LocalDate.parse(dateString, format)   // Parse the date from the string
println(date)                          // Output: 2020-06-24
```

## Deep Dive:

### Historical Context:

Parsing a date from a string has been a common task in programming for a long time. In the past, developers had to write their own algorithms to extract dates from strings. However, with the rise of high-level programming languages such as Kotlin, built-in classes and methods have made this task much easier and more efficient.

### Alternatives:

Apart from Kotlin, other programming languages also have standard libraries for parsing dates from strings. For example, in Java, the `SimpleDateFormat` class is commonly used for this purpose. There are also third-party libraries available, such as Joda-Time, that offer more advanced features for date parsing.

### Implementation Details:

The `DateFormat` and `DateTimeFormatter` classes use patterns consisting of letters and symbols to represent the format of a date. These patterns are defined in the official documentation and can be customized according to the specific format of the given date string.

## See Also:

- [DateFormat class in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-date-format/)
- [DateTimeFormatter class in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-date-time-formatter/)
- [Joda-Time library](https://www.joda.org/joda-time/)
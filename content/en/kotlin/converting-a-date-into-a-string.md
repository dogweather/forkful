---
title:                "Kotlin recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

As a developer, you might come across the need to convert dates into strings in your Kotlin programs. This can be useful for displaying dates in a specific format or for processing data. In this blog post, we will explore the reasons why you would need to do this and how to accomplish it.

## How To

To convert a date into a string in Kotlin, you can use the `format()` method of the `SimpleDateFormat` class. This takes in a date object and a format string as parameters and returns a string representation of the date in the specified format.

```Kotlin
val currentDate = Date()
val dateFormat = "dd-MMM-yyyy"

val formattedDate = SimpleDateFormat(dateFormat).format(currentDate)
println(formattedDate)
```

Running this code will output the current date in the format "dd-MMM-yyyy" (e.g. 14-Jan-2021).

You can also use the `DateTimeFormatter` class to format dates in Kotlin. This class provides more options for customizing the format of the date.

```Kotlin
val currentDate = LocalDate.now()
val dateFormat = "E dd MMM yyyy"

val formattedDate = currentDate.format(DateTimeFormatter.ofPattern(dateFormat))
println(formattedDate)
```

This code will output the current date in the format "Thu 14 Jan 2021".

## Deep Dive

Behind the scenes, the `format()` method uses the `java.util.Calendar` class to extract the date components and format them accordingly. The `SimpleDateFormat` class also supports various patterns for formatting the date, such as displaying the day of the week or the month in numeric or textual representation.

Additionally, you can use the `LocalDate` class to handle date objects without considering timezones or time. This can be useful if you only need to deal with dates and not worry about time values.

## See Also

- [SimpleDateFormat documentation](https://developer.android.com/reference/java/text/SimpleDateFormat.html)
- [DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Date and Time API in Kotlin](https://kotlinlang.org/docs/reference/datetime.html)
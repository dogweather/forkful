---
title:                "Getting the current date"
html_title:           "Kotlin recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in programming simply means retrieving the current date and time from the system. This is a common task for programmers as it allows them to incorporate time-sensitive tasks into their programs, such as displaying the current date or calculating the duration between two dates. Additionally, getting the current date is essential for creating time stamps, scheduling events, or simply tracking the passage of time in a program.

## How to:

To get the current date in Kotlin, we can use the built-in `java.time.LocalDate` class. First, we need to import the class by adding `import java.time.LocalDate` at the top of our code. Then, we can simply call the `now()` function on the `LocalDate` class, which will return the current date as a `LocalDate` object. Here's an example:

```Kotlin
import java.time.LocalDate

fun main(){
    val currentDate = LocalDate.now()
    print(currentDate)
}
```

The output of this code would be something like: `2021-10-01`, which represents the current date in the ISO format of year-month-day.

## Deep Dive:

Before Java 8, getting the current date in Java required additional code and was not as straightforward as it is now with Kotlin's `LocalDate` class. Prior to Java 8, the `java.util.Date` class was used, which had its own set of issues, such as not being thread-safe and not providing methods for handling time zones properly.

An alternative to using the `java.time.LocalDate` class in Kotlin is the `java.util.Calendar` class. However, this class also has its own set of problems, including being mutable and prone to errors.

In terms of implementation, the `now()` function in the `LocalDate` class uses the system's default time-zone to generate the current date. However, we can also use the `of()` function to specify a specific time-zone to get the current date for that region. 

## See Also:

To learn more about the `java.time.LocalDate` class and its different methods and available time zones, you can check out the official Kotlin documentation [here](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/). Additionally, you can also refer to the Java 8 `LocalDate` documentation [here](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html).
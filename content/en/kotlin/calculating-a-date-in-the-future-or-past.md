---
title:    "Kotlin recipe: Calculating a date in the future or past"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past can be a useful tool for various applications, such as event scheduling, payment due dates, or creating reminders. With Kotlin's date and time functionality, this task can be easily accomplished with just a few lines of code.

## How To

To calculate a date in the future or past, we first need to define the current date using the `LocalDate` class. We can do this by calling the `now()` method:

```Kotlin
val currentDate = LocalDate.now()
```

Next, we can use the `plus()` or `minus()` methods to add or subtract a certain number of days, weeks, or months from the current date, respectively. For example, to calculate a date 7 days in the future, we would do:

```Kotlin
val futureDate = currentDate.plusDays(7)
```

Similarly, if we want to calculate a date 2 weeks in the past, we would do:

```Kotlin
val pastDate = currentDate.minusWeeks(2)
```

We can also specify a specific date by using the `of()` method and passing in the year, month, and day as arguments. For example, to calculate a date 3 years in the future, we could do:

```Kotlin
val futureDate = LocalDate.of(currentDate.year + 3, currentDate.month, currentDate.day)
```

And finally, we can format the date in a specific way using the `format()` method. For example, to print the future date in the format of "MM/dd/yyyy", we could do:

```Kotlin
println("Future date: ${futureDate.format(DateTimeFormatter.ofPattern("MM/dd/yyyy"))}")
```

The output would be:

```
Future date: 05/17/2024
```

## Deep Dive

Kotlin's date and time functionality is built upon the `java.time` classes, which allow for more precise and accurate calculations compared to the traditional `java.util.Date` class. We can also manipulate the time component of a date by using the `LocalDateTime` class. Additionally, Kotlin provides support for time zone handling through the `java.time.ZonedDateTime` class.

When using the `plus()` or `minus()` methods, it is important to note that these operations do not modify the original date but instead return a new `LocalDate` object. This is due to the immutability of `LocalDate` objects in Kotlin.

## See Also

- Official Kotlin Documentation on Dates and Times: https://kotlinlang.org/docs/dates.html
- Java 8 Date and Time API: https://www.baeldung.com/java-8-date-time-intro
- Tutorial: Working with dates and times in Kotlin: https://proandroiddev.com/working-with-date-and-time-in-kotlin-30b8a211c90b
---
title:                "Kotlin recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may have encountered scenarios where you need to calculate a date in the future or past. This could be for tasks such as scheduling events, setting reminders, or calculating deadlines. Thankfully, with Kotlin, this process can be easily achieved using built-in date and time functions.

## How To

To calculate a date in the future or past in Kotlin, we can use the `LocalDate` class from the `java.time` package. We can create a `LocalDate` object by passing in the desired date as arguments for the `of()` method. For example:

```Kotlin
val currentDate = LocalDate.now() // Retrieves current date
val futureDate = LocalDate.of(2021, 9, 1) // Sets future date to September 1st, 2021
val pastDate = LocalDate.of(2020, 3, 15) // Sets past date to March 15th, 2020
```

We can then use the `plus()` and `minus()` methods to add or subtract the desired number of days, months, or years from the given date. For example:

```Kotlin
val newDate = currentDate.plusDays(7) // Adds 7 days to current date
val pastMonth = futureDate.minusMonths(1) // Subtracts 1 month from future date
val pastYear = pastDate.minusYears(3) // Subtracts 3 years from past date
```

We can also use `ChronoUnit` enum class to specify the unit of measurement for adding or subtracting time. For example:

```Kotlin
val newDate = currentDate.plus(2, ChronoUnit.WEEKS) // Adds 2 weeks to current date
val pastYear = pastDate.minus(5, ChronoUnit.YEARS) // Subtracts 5 years from past date
```

To get the final calculated date, we can use the `toString()` method to convert it to a readable format. For example:

```Kotlin
println("New date: $newDate")
```

This will print the result in the format of `yyyy-MM-dd` (e.g. 2021-09-08).

## Deep Dive

Under the hood, the `LocalDate` class uses the ISO-8601 calendar system to calculate dates. This means that it follows the standard of having 365 days in a year and 7 days in a week. It also takes into consideration leap years, so we don't need to worry about those additional days.

Furthermore, the `ChronoUnit` enum class provides options for adding or subtracting time in larger units such as weeks, months, and years, in addition to the smaller units like days, hours, and seconds. This gives us more flexibility and precision in calculating dates.

It's important to also note that the `plus()` and `minus()` methods return a new `LocalDate` object instead of modifying the original one. This is to ensure immutability and avoid any unexpected changes.

## See Also

- [Kotlin LocalDate class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Java 8 Date and Time API tutorial](https://www.baeldung.com/java-8-date-time-api)
- [Official Java documentation on ISO-8601 calendar system](https://docs.oracle.com/javase/8/docs/api/java/time/chrono/IsoChronology.html)

By using the techniques outlined above, you can easily calculate dates in the future or past in your Kotlin programs. This not only saves time but also ensures accuracy and consistency in your applications. Happy coding!
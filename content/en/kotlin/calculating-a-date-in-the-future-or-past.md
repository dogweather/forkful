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

## Why 

Calculating dates in the future or past can be helpful for a variety of reasons such as predicting project deadlines, scheduling events or appointments, or simply keeping track of important dates in the future.

## How To 

To calculate a date in the future or past using Kotlin, we can use the `java.time.LocalDate` class from the Java standard library. This class represents a date without time or time zone information. Here is an example of how to calculate a date 30 days in the future from today:

```Kotlin 
val currentDate = LocalDate.now() // get current date
val futureDate = currentDate.plusDays(30) // add 30 days to current date
println("Date in 30 days: $futureDate") // output: Date in 30 days: 2021-08-08
```

To calculate a date in the past, we can use the `minus` method instead. Here is an example of calculating a date 2 years in the past from today:

```Kotlin
val currentDate = LocalDate.now() // get current date
val pastDate = currentDate.minusYears(2) // subtract 2 years from current date
println("Date 2 years ago: $pastDate") // output: Date 2 years ago: 2019-07-09
```

Note that the `plus` and `minus` methods are polymorphic and can take different parameters such as `Days`, `Months`, or `Years`. 

## Deep Dive 

The `LocalDate` class provides various methods for manipulating dates including adding or subtracting time units, comparing dates, and checking for leap years. It also supports parsing and formatting dates using the `parse` and `format` methods, respectively.

Additionally, Kotlin provides an extension function `daysTo` which can be used to calculate the number of days between two `LocalDate` objects. Here is an example:

```Kotlin
val firstDate = LocalDate.of(2020, 10, 15) // create first date
val secondDate = LocalDate.of(2021, 10, 15) // create second date
val daysBetween = firstDate.daysTo(secondDate) // calculate days between
println("Days between two dates: $daysBetween") // output: Days between two dates: 365
```

## See Also 

- [Kotlin Language Reference](https://kotlinlang.org/docs/reference/)
- [Java LocalDate Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
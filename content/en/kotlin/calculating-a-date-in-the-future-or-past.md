---
title:                "Kotlin recipe: Calculating a date in the future or past"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Why 

Calculating a date in the future or past can be a useful tool for a variety of tasks. Whether you want to set a reminder for a future event or determine how many days have passed since a historical event, being able to calculate dates quickly and accurately can save you time and effort.

# How To

Calculating dates in Kotlin is simple and straightforward. All you need is the current date and the number of days you want to add or subtract. Let's take a look at some examples:

```Kotlin
// Calculate 10 days from today
val currentDate = LocalDate.now()
val futureDate = currentDate.plusDays(10)
println("In 10 days, the date will be: $futureDate")
```
Output: In 10 days, the date will be: 2022-01-10

```Kotlin
// Calculate 5 months from a specific date
val specificDate = LocalDate.of(2020, 12, 12)
val futureDate = specificDate.plusMonths(5)
println("In 5 months from 2020-12-12, the date will be: $futureDate")
```
Output: In 5 months from 2020-12-12, the date will be: 2021-05-12

You can also calculate dates in the past by using the `minus` methods instead of `plus`:

```Kotlin
// Calculate 2 weeks before a specific date
val specificDate = LocalDate.of(2019, 7, 10)
val pastDate = specificDate.minusWeeks(2)
println("2 weeks before 2019-07-10, the date was: $pastDate")
```
Output: 2 weeks before 2019-07-10, the date was: 2019-06-26

# Deep Dive

Behind the scenes, Kotlin uses the `java.time` library to handle date calculations. This library provides a variety of methods for working with dates and times, making it a powerful tool for any developer.

Some other useful methods for calculating dates in the future or past include `plusYears()`, `plusHours()`, and `minusDays()`. These methods allow you to specify units other than days.

It's also important to note that you can chain multiple methods together to perform multiple calculations on the same date. For example:

```Kotlin
// Calculate 5 years and 3 months from a specific date
val specificDate = LocalDate.of(2010, 5, 10)
val futureDate = specificDate.plusYears(5).plusMonths(3)
println("In 5 years and 3 months from 2010-05-10, the date will be: $futureDate")
```
Output: In 5 years and 3 months from 2010-05-10, the date will be: 2015-08-10

# See Also

If you want to learn more about working with dates in Kotlin, check out these helpful resources:

- [Kotlin documentation on the `java.time` library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html)
- [Tutorial on date and time in Kotlin](https://www.baeldung.com/kotlin-dates)
- [Kotlin Date and Time Cheat Sheet](https://devhints.io/kotlin-datetime)
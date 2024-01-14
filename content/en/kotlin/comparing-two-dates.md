---
title:    "Kotlin recipe: Comparing two dates"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why
Comparing dates is a common task in software development, especially when working with data that involves time-sensitive information. By comparing dates, we can determine the order of events or calculate the time difference between two events. In this blog post, we will be exploring how to compare two dates in Kotlin and the different ways to achieve this.

## How To
In Kotlin, there are multiple ways to compare two dates. Let's start with the most straightforward method using the built-in `compareTo()` function. This function returns an integer value to represent the relationship between two dates. If the first date is before the second date, it will return a negative number. If the second date is before the first date, a positive number is returned. And if both dates are equal, it will return 0.

To use the `compareTo()` function, we first need to create two `LocalDate` objects representing our dates. Then, we can call the function on one date and pass in the other date as a parameter. Let's see this in action:

```Kotlin
val date1 = LocalDate.of(2021, 9, 10)
val date2 = LocalDate.of(2021, 9, 15)

val result = date1.compareTo(date2)
println(result) // Output: -5
```

In this example, we can see that the first date (`date1`) is before the second date (`date2`) as the result is a negative number.

Another way to compare dates is by using the `isBefore()` and `isAfter()` functions. These functions return a Boolean value, indicating whether one date is before or after the other. Let's take a look at an example:

```Kotlin
val date1 = LocalDate.of(2021, 9, 10)
val date2 = LocalDate.of(2021, 9, 15)

val before = date1.isBefore(date2)
val after = date1.isAfter(date2)

println(before) // Output: true
println(after) // Output: false
```

Here, we can see that `before` is set to `true` since `date1` is before `date2` while `after` is set to `false` as `date1` is not after `date2`.

## Deep Dive
So far, we have only compared dates based on their chronological order, but there are other elements that we can take into consideration as well. For example, we might want to compare two dates based on the time or time zone. In these cases, we can use the `isBefore()` and `isAfter()` functions, passing in a `ChronoLocalDateTime` object representing both the date and time, or a `ChronoZonedDateTime` object representing both the date and time in a specific time zone.

We can also compare dates based on specific fields, such as year, month, or day of the month, using the `isBefore()` and `isAfter()` functions with a `ChronoLocalDate` object. This allows us to be more specific in our comparisons and extract only the information we need.

## See Also
- [Kotlin - Comparing Dates](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/kotlin.-comparable/compareTo.html)
- [Java 8 LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Java 8 ZonedDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
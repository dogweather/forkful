---
title:                "Kotlin recipe: Comparing two dates"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Determining the relationship between two dates is a common task in programming, especially when working with data that contains timestamps. Whether you need to check if a date is before or after another date, or calculate the difference between two dates, understanding how to compare dates is an essential skill for any Kotlin programmer.

## How To

To compare two dates in Kotlin, we can use the `compareTo()` method. This method returns an integer value that indicates the relationship between the two dates. Let's take a look at a simple example:

```Kotlin
val date1 = LocalDate.of(2020, 10, 8)
val date2 = LocalDate.of(2020, 10, 10)
val relationship = date1.compareTo(date2)

println(relationship) // Output: -2
```

In this example, we have two `LocalDate` objects representing October 8th and 10th of 2020. We then use the `compareTo()` method to check their relationship and store the result in the `relationship` variable. According to the [Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#compareTo-java.time.LocalDate-), the integer value returned by this method will be negative if the first date is before the second date, zero if they are equal, and positive if the first date is after the second date. Therefore, in our example, the output is -2, indicating that date1 is two days before date2.

We can also compare dates based on other units of time, such as months and years. For example:

```Kotlin
val date1 = LocalDate.of(2020, 10, 8)
val date2 = LocalDate.of(2020, 11, 8)
val monthDifference = date1.compareTo(date2, ChronoUnit.MONTHS)

println(monthDifference) // Output: -1
```

In this example, we use the `compareTo()` method with the `ChronoUnit.MONTHS` parameter, which compares the two dates based on their difference in months. The output, -1, indicates that date1 is one month before date2.

## Deep Dive

Under the hood, the `compareTo()` method uses the `isBefore()` and `isAfter()` methods to compare the dates. These methods, along with the `isEqual()` method, are used to check the specific relationship between two dates. For example, we can use `isBefore()` to check if a date is before another date:

```Kotlin
val date1 = LocalDate.of(2020, 10, 8)
val date2 = LocalDate.of(2020, 10, 10)

println(date1.isBefore(date2)) // Output: true
```

We can also use `isEqual()` to check if two dates are equal, regardless of the time:

```Kotlin
val date1 = LocalDateTime.of(2020, 10, 8, 10, 30)
val date2 = LocalDateTime.of(2020, 10, 8, 12, 0)

println(date1.isEqual(date2)) // Output: true
```

Understanding these methods can be useful when building more complex date comparison logic.

## See Also

- [Java Documentation for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/index.html)
---
title:                "Kotlin recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to compare two dates in your Kotlin program? Maybe you were trying to figure out which date comes before the other, or if they are the same date. Whatever the reason, comparing dates is a common task in programming and can be easily accomplished with Kotlin's built-in tools.

## How To
To compare two dates in Kotlin, we first need to create instances of the `LocalDate` class for each date. This class represents a date without a time or time zone. We can do this by using the `of` method and passing in the year, month, and day as arguments.

```Kotlin
val date1 = LocalDate.of(2021, 10, 15)
val date2 = LocalDate.of(2021, 10, 20)
```

Once we have our two dates, we can use the `isBefore`, `isAfter`, or `isEqual` methods to compare them. These methods return a boolean indicating if the first date is before, after, or equal to the second date.

```Kotlin
println(date1.isBefore(date2)) // Output: true
println(date2.isAfter(date1)) // Output: true
println(date1.isEqual(date2)) // Output: false
```

We can also use the `compareTo` method, which returns an `Int` indicating the order of the two dates. If the first date is before the second date, it will return a negative number. If the first date is after the second date, it will return a positive number. If the two dates are equal, it will return 0.

```Kotlin
println(date1.compareTo(date2)) // Output: -5
println(date2.compareTo(date1)) // Output: 5
println(date1.compareTo(date1)) // Output: 0
```

## Deep Dive
Under the hood, the methods we used to compare dates are converting the dates into a standardized unit called "epoch days". This is the number of days since January 1, 1970. This allows for easier comparison and calculation of dates.

It's also important to note that these methods only work with dates and do not consider time or time zones. If you need to compare dates and times, you can use the `LocalDateTime` class instead.

## See Also
- [Kotlin Date and Time Classes](https://kotlinlang.org/docs/datetime.html)
- [Understanding Epoch Time](https://www.unixtimestamp.com/)
- [Comparing Dates in Java](https://www.baeldung.com/java-comparing-dates)
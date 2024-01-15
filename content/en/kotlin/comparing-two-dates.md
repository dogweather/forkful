---
title:                "Comparing two dates"
html_title:           "Kotlin recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in programming, especially when dealing with time-sensitive data. With the help of Kotlin, you can easily compare two dates and perform operations on them. This can be useful for tasks like sorting dates, detecting overlaps, or calculating time differences. 

## How To

To compare two dates in Kotlin, we can use the `compareTo()` method provided by the `Date` class. Let's take a look at an example:

```Kotlin
val date1 = Date(2021, 9, 1)
val date2 = Date(2021, 9, 15)

if (date1.compareTo(date2) == 0) {
   println("Both dates are equal")
} else if (date1.compareTo(date2) < 0) {
   println("Date 1 comes before Date 2")
} else {
   println("Date 2 comes before Date 1")
}
```

Here, we create two `Date` objects with different values and use the `compareTo()` method to compare them. This method returns an integer value based on the comparison, with 0 indicating equality, a negative value indicating that the first date comes before the second, and a positive value indicating the opposite. 

We can also use other methods like `after()` and `before()` to compare dates based on their temporal relationship. Here's an example:

```Kotlin
val date1 = Date(2021, 9, 1)
val date2 = Date(2021, 9, 15)

if (date1.after(date2)) {
   println("Date 1 is after Date 2")
} else if (date1.before(date2)) {
   println("Date 1 is before Date 2")
} else {
   println("Both dates are equal")
}
```

The `after()` method returns `true` if the first date comes after the second, while `before()` returns `true` if the first date comes before the second. 

## Deep Dive

Internally, dates in Kotlin are represented as `Long` values denoting the number of milliseconds since January 1, 1970, 00:00:00 UTC. This means that comparing dates in Kotlin is essentially comparing two numerical values. 

When comparing dates, the time zone also plays a crucial role. The `Date` class uses the default time zone of the system, but you can set a specific time zone using the `TimeZone` class. Additionally, Kotlin provides the `Calendar` class for more advanced date and time operations. 

## See Also

- [Kotlin Date and Time API](https://kotlinlang.org/docs/datetime.html)
- [Kotlin Date Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/index.html)
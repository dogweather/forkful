---
title:    "Kotlin recipe: Comparing two dates"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When working with dates in a Kotlin program, it is often necessary to compare two different dates. This could be for tasks such as checking if a certain date has passed or determining which of two dates is earlier. By understanding how to compare dates in Kotlin, you can ensure that your program accurately handles all types of date comparisons.

## How To

To compare two dates in Kotlin, we will use the `compareTo()` method. This method is available on the `LocalDate` class and allows us to compare two dates based on their chronological order. Let's see this in action with some code examples:

```Kotlin
// Create two date objects
val date1 = LocalDate.of(2021, 10, 10)
val date2 = LocalDate.of(2021, 10, 11)

// Compare the dates using the compareTo() method
val result = date1.compareTo(date2)

// Output: -1, which means that date 1 comes before date 2
println(result)
```

In this example, we create two `LocalDate` objects, `date1` and `date2`, representing October 10th and 11th of 2021. We then use the `compareTo()` method to compare them, which returns an integer value representing the chronological order of the dates. In this case, -1 is returned, indicating that `date1` comes before `date2`.

We can also use the `compareTo()` method to compare dates based on equality. Let's see this with another example:

```Kotlin
// Create two date objects
val date1 = LocalDate.of(2021, 4, 1)
val date2 = LocalDate.of(2021, 4, 1)

// Compare the dates using the compareTo() method
val result = date1.compareTo(date2)

// Output: 0, which means that date 1 and date 2 are equal
println(result)
```

In this example, both `date1` and `date2` are representing the same date, April 1st of 2021. When we use the `compareTo()` method to compare them, it returns 0, indicating that the two dates are equal.

## Deep Dive

Under the hood, the `compareTo()` method is using the `compareTo()` method from the `Comparable` interface. This interface allows objects to be compared and sorted based on their natural order. In the case of `LocalDate` objects, this order is chronologically. If you want to compare dates based on a different criteria, you can also use the `compare()` method from the `Comparator` interface.

Additionally, the `compareTo()` method in Kotlin also accounts for different time zones and daylight savings time. This ensures that your date comparisons are accurate and take into consideration the potential differences in time zones.

## See Also

- [Official Kotlin documentation on LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Java documentation on the Comparable interface](https://docs.oracle.com/javase/8/docs/api/java/lang/Comparable.html)
- [Java documentation on the Comparator interface](https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html)
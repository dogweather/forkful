---
title:    "Kotlin recipe: Calculating a date in the future or past"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why 
Calculating dates in the future or past can be extremely useful in a variety of programming scenarios. For example, in event management systems, it is necessary to calculate the date of upcoming events or schedule reminders for important dates. In this blog post, we will explore how to do this using Kotlin programming language.

## How To 
To calculate a date in the future or past, we can utilize the built-in `Calendar` class in Kotlin. This class provides various methods and properties to manipulate dates and times. Let's take a look at a simple example:

```Kotlin
// Get current date
val currentDate = Calendar.getInstance()

// Add 1 month to current date
currentDate.add(Calendar.MONTH, 1)

// Print future date
println("Future date: ${currentDate.time}")
```
The output of the code above will be the date of 1 month from the current date. Simple, right? We can also subtract time units, such as days, hours, or even milliseconds, by using `add()` method with a negative value.

```Kotlin 
// Get current date
val currentDate = Calendar.getInstance()

// Subtract 2 days from current date
currentDate.add(Calendar.DAY_OF_MONTH, -2)

// Print past date
println("Past date: ${currentDate.time}")
```

We can also set specific dates by using the `set()` method. For example, if we want to set a specific date, say, June 30, 2022, we can do it like this:

```Kotlin 
// Get current date
val currentDate = Calendar.getInstance()

// Set date to June 30, 2022
currentDate.set(2022, Calendar.JUNE, 30)

// Print set date
println("Set date: ${currentDate.time}")
```

## Deep Dive 
If we want to get more accurate calculations, we can use the `java.time` API introduced in Java 8, which is fully compatible with Kotlin. This API provides a `LocalDate` class, which represents a date without a specific time or timezone. Let's see how we can use it to calculate exact dates in the future or past:

```Kotlin
// Get current date
val currentDate = LocalDate.now()

// Add 3 weeks to current date
val futureDate = currentDate.plusWeeks(3)

// Subtract 1 year from current date
val pastDate = currentDate.minusYears(1)

// Print future and past dates
println("Future date: $futureDate")
println("Past date: $pastDate")
```

The above code will output the date of 3 weeks from the current date and the date of 1 year ago, respectively. The `LocalDate` class also provides methods to calculate dates based on specific timezones and to handle daylight saving time.

## See Also 
- [Kotlin - Date and Time API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time/index.html)
- [Java 8 Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Android Developers - Date and Time Basics](https://developer.android.com/guide/topics/icalendar)

With the help of the examples and tips provided in this blog post, calculating dates in the future or past using Kotlin should now be a breeze. Happy coding!
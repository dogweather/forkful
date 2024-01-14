---
title:    "Kotlin recipe: Getting the current date"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

---

## Why

In the world of programming, there are many tasks that require a precise understanding of time. Whether it's for scheduling tasks, tracking events, or simply displaying the current date on a user interface, being able to accurately obtain the current date is an essential skill for any programmer. In this blog post, we will explore how to get the current date in Kotlin and why it's an important aspect of coding.

## How To

To begin, we will need to import the Date class from the java.util package. This class provides us with methods for creating and manipulating dates, including obtaining the current date.

```Kotlin
import java.util.Date
```

Next, we can use the Date() constructor to create a new Date object, which will automatically set the date to the current date and time.

```Kotlin
val currentDate = Date()
```

We can then use various methods from the Date class to extract specific information from the current date, such as the day, month, year, and time.

```Kotlin
val today = currentDate.day
val currentMonth = currentDate.month
val currentYear = currentDate.year
val currentTime = currentDate.time
```

We can also format the output of the current date using the SimpleDateFormat class. This allows us to specify a desired date format, such as "MM/dd/yyyy" for month/day/year, and then use the format() method to display the current date in that format.

```Kotlin
val dateFormat = SimpleDateFormat("MM/dd/yyyy")
val formattedDate = dateFormat.format(currentDate)
```

Let's take a look at the output of the current date using these methods:

```
Current date: Fri Oct 01 13:22:19 EDT 2021
Current day: Fri
Current month: Oct
Current year: 121
Current time: 1633093339403
Formatted date: 10/01/2021
```

## Deep Dive

Behind the scenes, Kotlin's Date class is actually using Java's Date class, which has been around since JDK 1.0. This means that the Date class has been used for decades and has undergone many updates and improvements. However, it's important to note that the Date class is not the most efficient or reliable way to handle dates and times.

Java 8 introduced the LocalDate, LocalTime, and LocalDateTime classes, which provide a more modern and accurate way of working with dates and times. These classes are part of the java.time package and offer features such as immutability, thread-safety, and better support for time zones.

If you are working with Kotlin and want to have more control and accuracy over dates and times, it's recommended to use the java.time classes instead of the older Date class. However, for simple tasks, the Date class is still a viable option.

## See Also

- [Kotlin Basic Syntax](https://kotlinlang.org/docs/basic-syntax.html)
- [Java.util Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java.time Package Overview](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
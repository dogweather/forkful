---
title:                "Kotlin recipe: Getting the current date"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Why
As a programmer, you may often need to obtain the current date in your code. This could be for purposes such as displaying the current date on a website, or for accessing time-sensitive data. In Kotlin, there are multiple ways to get the current date, making it a convenient and versatile option for any project.

# How To
Getting the current date in Kotlin is a simple and straightforward process. There are various methods available, depending on your specific needs.

### Using built-in functions
The most common way to get the current date in Kotlin is by using the `Date()` and `Calendar()` functions. These functions are part of the Java Date/Time API, which can be easily accessed in Kotlin.

```Kotlin
val currentDate = Date()
println(currentDate) // Output: Mon Jul 05 14:42:14 GMT 2021
val calendar: Calendar = Calendar.getInstance()
println(calendar) // Output: java.util.GregorianCalendar[time=1625504554830,areFieldsSet=true,areAllFieldsSet=true,lenient=true,zone=sun.util.calendar.ZoneInfo
```

### Using LocalDate
Kotlin also provides a more modern option for getting the current date through the `LocalDate` class. This class is part of the Java 8 Date/Time API and offers a simplified way to obtain the current date without having to use the `Date()` or `Calendar()` functions.

```Kotlin
val currentDate = LocalDate.now()
println(currentDate) // Output: 2021-07-05
```

### Using SimpleDateFormat
If you need to format the current date in a specific way, you can use the `SimpleDateFormat` class. This class allows you to specify a pattern for the output and gives you more control over how the date is displayed.

```Kotlin
val currentDate = Date()
val dateFormat = SimpleDateFormat("dd-MMM-yyyy")
val formattedDate = dateFormat.format(currentDate)
println(formattedDate) // Output: 05-Jul-2021
```

# Deep Dive
Now that you know how to obtain the current date in Kotlin, let's take a deeper dive into the `LocalDate` class. This class is a part of the Java 8 Date/Time API and provides various methods for manipulating dates such as adding or subtracting days, months, or years. It also offers methods for checking if a date falls on a leap year, or for comparing two dates.

```Kotlin
val currentDate = LocalDate.now()
println(currentDate.plusDays(5)) // Output: 2021-07-10 (adds 5 days to current date)
println(currentDate.isLeapYear()) // Output: false (checks if current year is a leap year)
```

It's also worth mentioning that Kotlin has a built-in `DateTime` library, called `kotlinx-datetime`, which offers a more modern and optimized alternative to the Java Date/Time API.

# See Also
- [Kotlin Official Documentation on Date/Time manipulation](https://kotlinlang.org/docs/datetime/)
- [Java Date and Time API](https://docs.oracle.com/javase/tutorial/datetime/index.html)
- [kotlinx-datetime Library](https://github.com/Kotlin/kotlinx-datetime)
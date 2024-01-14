---
title:                "Kotlin recipe: Converting a date into a string"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is an essential task in programming. Dates are an important aspect of our daily lives, and being able to work with them in a string format allows us to display and manipulate them in various ways. In this blog post, we will explore why converting dates into strings is useful and how to do it in Kotlin.

## How To

Converting a date into a string in Kotlin is a simple and straightforward process. Let's take a look at an example where we have a date object and we want to convert it into a string in the format of "MM/dd/yyyy".

```Kotlin
val date = Date() //get current date
val dateFormat = SimpleDateFormat("MM/dd/yyyy") //create date format
val dateString = dateFormat.format(date) //convert date to string
println("Today's date is: $dateString") //output: Today's date is: 10/15/2021
```

In the above code, we first create a date object using the `Date()` constructor. Then, we create a `SimpleDateFormat` object with the desired date format. Finally, we use the `format()` method to convert the date object into a string and store it in the `dateString` variable. We can then use this string in our application as needed.

We can also specify the timezone and locale in the `SimpleDateFormat` constructor to get the date in a specific time or language format. For example, if we want to get the date in French with the current time, we can use the following code:

```Kotlin
val date = Date()
val dateFormat = SimpleDateFormat("MM/dd/yyyy HH:mm:ss", Locale.FRENCH)
dateFormat.timeZone = TimeZone.getDefault() //get timezone of device
val dateString = dateFormat.format(date)
println("Aujourd'hui, c'est le: $dateString") //output: Aujourd'hui, c'est le: 15/10/2021 15:39:52
```

## Deep Dive

Behind the scenes, Kotlin uses the `Date#toString()` method to convert a date into a string. This method returns the date in a specific format that depends on the default locale and timezone of the device. However, using the `SimpleDateFormat` class allows us to have more control over the format of the date string.

It is important to note that the `Date` class is considered to be outdated and has been replaced by the new `java.time` package in Java 8. In Kotlin, we can use the `java.time.LocalDate` class to represent a date without a time and the `java.time.LocalDateTime` class to represent a date with a time. These classes have built-in methods for formatting dates into strings. For example:

```Kotlin
val currentDate = LocalDate.now() //get current date
val dateString = currentDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
println("Today's date is: $dateString") //output: Today's date is: 15/10/2021
```

## See Also

- [Kotlin Official Documentation on Date and Time](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date/)
- [Java 8 Date-Time API Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [How to Convert Java 8 Date Time to String](https://howtodoinjava.com/java/date-time/convert-java8-date-time-to-string/)
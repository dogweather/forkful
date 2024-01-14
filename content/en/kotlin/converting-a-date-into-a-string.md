---
title:    "Kotlin recipe: Converting a date into a string"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a common task in programming, especially when dealing with user interfaces or data storage. In Kotlin, there are several ways to achieve this and understanding the process can greatly improve the readability and functionality of your code.

## How To

Converting a date into a string can be done using Kotlin's built-in functions for date formatting. Here is an example:

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDateTime = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("MM/dd/yyyy")
    val formattedDate = currentDateTime.format(formatter)
    println("Current date is: $formattedDate")
}
```
Output:
```
Current date is: 08/06/2021
```
In the above code, we first import the necessary packages. Then, we use the `LocalDateTime` class to get the current date and time. Next, we create a `DateTimeFormatter` object and specify the desired formatting using the `ofPattern` function. Finally, we use the `format` function to convert the date into a string according to the specified format.

Another way to convert a date into a string is by using the `SimpleDateFormat` class. Here is an example:

```Kotlin
import java.util.*

fun main() {
    val currentDate = Calendar.getInstance().time
    val dateFormat = SimpleDateFormat("dd MMMM yyyy")
    val formattedDate = dateFormat.format(currentDate)
    println("Current date is: $formattedDate")
}
```
Output:
```
Current date is: 06 August 2021
```
In this code, we use the `Calendar` class to get the current date. Then, we create a `SimpleDateFormat` object and specify the desired date format using the `format` function.

## Deep Dive

Under the hood, date objects are stored as a specific number of milliseconds since January 1, 1970, also known as the Unix Epoch. Converting these milliseconds into a human-readable date can be done using different patterns and formats. In Kotlin, the `DateTimeFormatter` and `SimpleDateFormat` classes provide a convenient and flexible way to achieve this.

It is also worth noting that Kotlin's `LocalDateTime` and `SimpleDateFormat` classes are designed to be thread-safe, making them ideal for use in a multithreaded environment.

## See Also

- [Date and Time formatting in Kotlin](https://kotlinlang.org/docs/datetime.html#formatting)
- [Converting Dates and Times in Kotlin](https://www.baeldung.com/kotlin/convert-dates)
- [Java SimpleDateFormat documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
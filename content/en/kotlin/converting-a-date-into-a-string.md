---
title:    "Kotlin recipe: Converting a date into a string"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why
Converting a date into a string is a common task in programming, especially when working with user interfaces or databases. By converting a date into a string, we can display the date in a more user-friendly format and manipulate it as needed.

## How To
To convert a date into a string in Kotlin, we can use the `format` method from the `kotlin.text` package. Here is an example:

```Kotlin
// creating a Date object
val date = Date()
// formatting the date into a string
val dateString = date.format("dd/MM/yyyy")
// printing the converted string
println(dateString)
```
The output of the above code would be something like `21/05/2021`.

We can also customize the format of the string by using the appropriate symbols. For example, if we want to include the time as well, we can use the format `HH:mm:ss`. Here is an example:

```Kotlin
// formatting the date and time into a string
val dateTimeString = date.format("dd/MM/yyyy HH:mm:ss")
// printing the converted string
println(dateTimeString)
```
The output would be something like `21/05/2021 14:30:45`.

## Deep Dive
The `format` method in Kotlin uses the same format as the `SimpleDateFormat` class in Java, which follows the `strftime` standard. This means we can use the same symbols as in Java for formatting the date and time. Some commonly used symbols are:

- `dd` for day of the month
- `MM` for month
- `yyyy` for year
- `HH` for hour (in 24-hour format)
- `mm` for minutes
- `ss` for seconds
- `a` for AM/PM marker

For a complete list of all the symbols, you can refer to the [documentation](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html).

It's also worth mentioning that the `format` method is an extension function and can be called directly on a `Date` object. This makes the code more concise and readable.

## See Also
- [Kotlin documentation on Date and Time](https://kotlinlang.org/docs/datetime.html)
- [Java documentation on SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
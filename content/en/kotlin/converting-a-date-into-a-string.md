---
title:                "Converting a date into a string"
html_title:           "Kotlin recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a simple task, but it can be a common occurrence in programming applications. Having a clear understanding of how to do this can save you time and effort in the long run.

## How To

To convert a date into a string in Kotlin, we can use the built-in `toString()` function. Let's take a look at an example:

```Kotlin
val date = Date() // create a Date object
val dateString = date.toString() // convert the date into a string
println(dateString) // output: Sat Sep 18 22:14:42 EDT 2021
```

In the above code, we first create a `Date` object using the `Date()` constructor. Then, we use the `toString()` function to convert the date into a string and assign it to the `dateString` variable. Finally, we print the `dateString` to the console, which outputs the date in a string format.

We can also specify a format for the date string using the `SimpleDateFormat` class. Let's see an example of that:

```Kotlin
val date = Date() // create a Date object
val sdf = SimpleDateFormat("dd-MM-yyyy") // specify the desired format
val dateString = sdf.format(date) // convert the date into a string with the specified format
println(dateString) // output: 18-09-2021
```

In this code, we create a `SimpleDateFormat` object and specify the desired date format in the constructor. Then, we use the `format()` function to convert the date into a string with the specified format and assign it to the `dateString` variable. Finally, we print the `dateString` to the console, which outputs the date in the specified format.

## Deep Dive

The `toString()` function uses the default locale and timezone to format the date into a string. However, we can also specify a specific locale and timezone for the date string by passing it as a parameter in the `toString()` function. For example:

```Kotlin
val date = Date() // create a Date object
val locale = Locale("fr", "FR") // specify the desired locale (France)
val timezone = TimeZone.getTimeZone("Europe/Paris") // specify the desired timezone (Central European Time)
val dateString = date.toString(locale, timezone) // convert the date into a string with the specified locale and timezone
println(dateString) // output: sam. sept. 18 22:14:42 CEST 2021
```

In this code, we create a `Locale` object for France and a `TimeZone` object for Central European Time. Then, we pass in these objects as parameters in the `toString()` function to format the date string accordingly.

## See Also

- [Kotlin Date class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Kotlin SimpleDateFormat class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/)
---
title:    "Kotlin recipe: Getting the current date"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
As a programmer, it's important to be able to retrieve the current date in order to track and organize data, perform time-based calculations, or simply display the current date to the user. In Kotlin, obtaining the current date is a simple and straightforward process.

## How To
To get the current date in Kotlin, we can use the `LocalDate.now()` function. This function returns the current date as a `LocalDate` object, which represents a date without a time or timezone. Let's see an example:

```
Kotlin
val currentDate = LocalDate.now()
println(currentDate)
```

In this code block, we declared a variable `currentDate` and assigned it the value returned by `LocalDate.now()`, which is the current date. We then used `println()` to print the value of `currentDate` to the console. The output would be in the format `YYYY-MM-DD`, for example `2021-03-25`.

We can also get the current date with a specific timezone using the `ZonedDateTime.now()` function. This function returns a `ZonedDateTime` object, which represents a date and time with a specific timezone. Let's see an example:

```
Kotlin
val currentDate = ZonedDateTime.now(ZoneId.of("America/New_York"))
println(currentDate)
```

In this code block, we used the `now()` function with a specified timezone, in this case, "America/New_York". The output would be in the format `YYYY-MM-DDThh:mm:ssTZ`, for example `2021-03-25T19:30:15-04:00`.

## Deep Dive
Under the hood, the `LocalDate.now()` and `ZonedDateTime.now()` functions use the `Clock` class to obtain the current date and time. This class uses the system's default time-zone and clock to retrieve the current date and time. However, we can also pass a specific `Clock` object as a parameter to these functions to get the current date and time with a custom clock. This can be useful for testing purposes.

Additionally, the `LocalDateTime.now()` function can be used to obtain the current date and time as a `LocalDateTime` object, which represents a date and time without a timezone.

## See Also
- [Java 8 Time API](https://www.baeldung.com/java-8-date-time-intro)
- [Kotlin Standard Library Functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-functions/index.html)
- [Clock class documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Clock.html)
---
title:                "Kotlin recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to know the current date in your Kotlin program? Maybe you want to display it to the user, use it for calculations, or simply keep track of when your code was last run. Whatever the reason, getting the current date is a common task in programming. In this blog post, we'll explore how to do just that in Kotlin.

## How To
To get the current date in Kotlin, we can use the built-in `LocalDate` class from the `java.time` package. Here's an example of how to use it:

```Kotlin
import java.time.LocalDate

val currentDate = LocalDate.now()
println("Today's date is: $currentDate")
```

The code above imports the `java.time.LocalDate` class and uses the `now()` method to get the current date. We then use string interpolation to display the date in a user-friendly format. Running this code will give the following output:

```
Today's date is: 2021-06-17
```

But what if we want to customize the format of the date? For that, we can use `DateTimeFormatter` from the same package. Here's an example:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val currentDate = LocalDate.now()
val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
val formattedDate = currentDate.format(formatter)
println("Today's date in dd-MM-yyyy format is: $formattedDate")
```

Running this code will give the following output:

```
Today's date in dd-MM-yyyy format is: 17-06-2021
```

As you can see, by using `DateTimeFormatter` we can easily change the format of the date to suit our needs. You can explore different patterns to customize the date format even further.

## Deep Dive
Behind the scenes, the `LocalDate` class is using the timezone of the system to get the current date. If you want to specify a different timezone, you can use the `now` method that takes a `ZoneId` as a parameter. Additionally, the `LocalDate` class also provides methods to retrieve specific components of the date, such as day, month, and year. You can check out the official documentation for more details on these methods.

## See Also
Here are some additional resources to learn more about getting the current date in Kotlin:

- [Kotlin documentation on `java.time` package](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time/)
- [Blog post on formatting dates in Kotlin](https://www.baeldung.com/kotlin-datetime-format)
- [YouTube tutorial on working with dates in Kotlin](https://www.youtube.com/watch?v=MIkNCSdxiKk)
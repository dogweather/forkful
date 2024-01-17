---
title:                "Calculating a date in the future or past"
html_title:           "Kotlin recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past is a common task among programmers. It involves manipulating dates to determine a specific date and time in the future or past based on a given initial date. This is useful in a variety of applications, such as scheduling events or generating reports.

## How to:

Calculating a date in the future or past can be accomplished using the built-in functions provided by the ```java.util.Calendar``` class in Kotlin. These functions allow us to add or subtract a specific number of years, months, days, hours, minutes, or seconds from a given date. For example, to calculate the date two years from now, we can use the ```add()``` function like this:

```Kotlin
val calendar = Calendar.getInstance() //get current date
calendar.add(Calendar.YEAR, 2) //add 2 years
println("Two years from now is on ${calendar.time}") //print the calculated date
```

This will output: ```Two years from now is on Sun Apr 18 15:42:55 EDT 2021```

For calculating a date in the past, we can use the ```roll()``` function instead of ```add()```. The ```roll()``` function only adjusts the specified fields without changing the larger fields. For example, to calculate the date one month ago, we can use the ```roll()``` function like this:

```Kotlin
val calendar = Calendar.getInstance() //get current date
calendar.roll(Calendar.MONTH, false) //roll back 1 month
println("One month ago was on ${calendar.time}") //print the calculated date
```

This will output: ```One month ago was on Fri Feb 18 15:42:55 EST 2021```

## Deep Dive:

Programmers have been calculating dates for centuries, long before the advent of computers. In the past, it was done manually or with the help of a calendar. Fortunately, with advancements in technology, we now have built-in functions and libraries to make date calculations easier and more accurate.

Apart from using the ```java.util.Calendar``` class, developers can also use other libraries like Joda Time or Java 8's new Date and Time API to perform date calculations. These libraries provide more extensive and precise methods for calculating dates.

When working with dates, it is important to consider time zones and daylight saving time to ensure accurate calculations. Different programming languages and libraries may handle these factors differently, so it's essential to understand the implementation details when using them.

## See Also:

- [java.util.Calendar documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Joda Time library](https://www.joda.org/joda-time/)
- [Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
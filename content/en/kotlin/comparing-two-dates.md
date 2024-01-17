---
title:                "Comparing two dates"
html_title:           "Kotlin recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is the process of determining if one date is before, after, or equal to another date. Programmers often compare dates when dealing with tasks such as sorting, filtering, or scheduling. By comparing dates, programmers can make decisions based on the chronological order of events or tasks.

## How to:

To compare two dates in Kotlin, we can use the built-in functions of the Date class. We will use two dates, date1 and date2, and compare them using the compareTo function. The function returns an integer value based on the comparison, with a value of 0 indicating that the two dates are equal.

```Kotlin
val date1 = Date(2021, 8, 1)
val date2 = Date(2021, 8, 15)

val comparison = date1.compareTo(date2)

println("Date1 is $comparison compared to Date2.")
```

The output for this code would be:
```
Date1 is -1 compared to Date2.
```
This means that date1 is before date2.

## Deep Dive:

When comparing dates, it is important to consider the time zone and daylight saving time. In Kotlin, the Date class is based on the Java Date class, which uses the UTC time zone by default. However, if the dates are based on different time zones, it can affect the comparison result. To avoid this issue, we can use the Calendar class to set the time zone before comparing dates.

Alternatively, we can use the Joda-Time library to compare dates in a specific time zone. This library allows for more customizable and accurate date comparisons.

## See Also:

- Kotlin Date class documentation: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/
- Java Date class documentation: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- Joda-Time library: https://www.joda.org/joda-time/
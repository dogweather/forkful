---
title:                "Calculating a date in the future or past"
html_title:           "Java recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many applications, from scheduling events to managing deadlines. Having the ability to accurately calculate and manipulate dates can greatly improve the functionality and efficiency of a program. 

## How To

To calculate a date in the future or past, we can use the `LocalDate` class from the `java.time` package. This class represents a date in the ISO-8601 calendar system (yyyy-MM-dd). 

We can create a `LocalDate` object for a specific date by passing in the year, month, and day as parameters. For example, if we want to represent January 1st, 2020, we can write:

```Java 
LocalDate date = LocalDate.of(2020, 1, 1);
```

To calculate a date in the future or past, we can use the `plus()` and `minus()` methods. These methods take in a `Period` object, which represents a period of time in years, months, and days. 

For example, if we want to calculate 10 days from our initial date, we can write:

```Java
LocalDate futureDate = date.plus(Period.ofDays(10));
```

Similarly, if we want to calculate 1 month and 3 years in the past, we can write:

```Java
LocalDate pastDate = date.minus(Period.ofYears(3).plusMonths(1));
```

We can also use the `plus()` and `minus()` methods with `ChronoUnit` to calculate dates in smaller units such as hours, minutes, or seconds. For example, to calculate 3 hours in the future, we can write:

```Java
LocalDate futureDate = date.plus(3, ChronoUnit.HOURS);
```

### Sample Output

| Code | Output |
|------|--------|
| `LocalDate date = LocalDate.of(2020, 1, 1);` | `2020-01-01` |
| `LocalDate futureDate = date.plus(Period.ofDays(10));` | `2020-01-11` |
| `LocalDate pastDate = date.minus(Period.ofYears(3).plusMonths(1));` | `2016-12-01` |
| `LocalDate futureDate = date.plus(3, ChronoUnit.HOURS);` | `2020-01-01T03:00` |

## Deep Dive

Under the hood, the `plus()` and `minus()` methods are using the `Temporal` interface, which represents a temporal amount. This interface is implemented by the `LocalDate` class and allows for manipulation of dates. 

Additionally, the `Period` class is used to represent a period of time in years, months, and days. It offers methods like `ofYears()`, `ofMonths()`, and `ofDays()` for easy creation of `Period` objects.

And to calculate dates in smaller units using `ChronoUnit`, we use the `plus()` and `minus()` methods that are specific to the `ChronoUnit` enum. This enum contains all the available units for date and time manipulation.

## See Also

- [Java 8 API - LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java 8 API - ChronoUnit](https://docs.oracle.com/javase/8/docs/api/java/time/temporal/ChronoUnit.html)
- [Understanding the Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
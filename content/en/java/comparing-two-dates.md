---
title:                "Comparing two dates"
html_title:           "Java recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

As a developer, it is important to properly compare dates in Java in order to ensure accurate data analysis and manipulation. This skill is crucial when working with time-sensitive applications or when handling data from multiple sources.

## How To

To compare two dates in Java, we will use the `LocalDate` class from the `java.time` package. This class provides useful methods for date comparison and manipulation.

First, we will create two `LocalDate` objects using the `of()` method and passing in the respective dates in `YYYY-MM-DD` format.

```
Java
LocalDate date1 = LocalDate.of(2021, 5, 10);
LocalDate date2 = LocalDate.of(2021, 8, 14);
```

We can now use the `isBefore()` and `isAfter()` methods to compare these dates. These methods return a boolean value, `true` if the first date is before or after the second date, and `false` if they are equal.

```
Java
date1.isBefore(date2); // returns true
date1.isAfter(date2);  // returns false
```

Similarly, we can use the `isEqual()` method to check if the two dates are equal.

```
Java
date1.isEqual(date2);  // returns false
```

We can also use the `compareTo()` method, which returns an integer value depending on the comparison result. If the first date is before the second date, it returns a negative number; if the dates are equal, it returns 0, and if the first date is after the second date, it returns a positive number.

```
Java
date1.compareTo(date2);  // returns -92
```

This method is useful when we need to sort dates in a specific order.

## Deep Dive

When comparing dates, we should also consider the time zone and daylight saving time (DST) changes. The `LocalDate` class only deals with dates, not time or time zones. To handle time and time zone differences, we can use the `ZonedDateTime` class from the same package.

We can use the `atStartOfDay()` method to get the start of the day in the system's default time zone. This will default to the local time zone but can be customized to a specific time zone using the `ZoneId` class.

```
Java
ZonedDateTime date1 = LocalDate.of(2021, 5, 10).atStartOfDay(); // default time zone
ZonedDateTime date2 = LocalDate.of(2021, 8, 14).atStartOfDay(ZoneId.of("Europe/Paris")); // customized time zone
```

We can then compare the `ZonedDateTime` objects using the same methods as before.

```
Java
date1.isBefore(date2); // returns true
date1.isAfter(date2);  // returns false
```

It is also important to consider DST changes when comparing dates. We can use the `withEarlierOffsetAtOverlap()` and `withLaterOffsetAtOverlap()` methods to handle ambiguous DST overlaps. These methods will adjust the date and time based on the earlier or later offset during an overlap.

```
Java
// Assuming DST changes on 2021-10-31 at 2:00AM in the local time zone
ZonedDateTime date1 = ZonedDateTime.of(2021, 10, 31, 1, 30, 0, 0, ZoneId.systemDefault());
ZonedDateTime date2 = ZonedDateTime.of(2021, 10, 31, 3, 30, 0, 0, ZoneId.systemDefault());
date1.compareTo(date2);  // returns 0
date1 = date1.withEarlierOffsetAtOverlap();
date1.compareTo(date2);  // returns -1
```

## See Also

- [Java Documentation on Comparing Dates](https://docs.oracle.com/javase/tutorial/datetime/iso/meaning.html)
- [ZoneId Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/ZoneId.html)
- [ZonedDateTime Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
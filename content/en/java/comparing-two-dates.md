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

## What & Why?
When working with dates in Java, it is often necessary to compare two dates to check for equality or determine which date is earlier or later. Comparing dates allows programmers to perform tasks such as sorting, filtering, and scheduling based on date and time information. It is an essential skill for building any type of time-sensitive application.

## How to:
Comparing two dates in Java is a fairly straightforward task. The `java.util.Date` class provides methods for comparing dates using a variety of criteria. Let's take a look at a few examples:

```Java
// create two date objects
Date date1 = new Date();
Date date2 = new Date();

// compare for equality using the equals() method
System.out.println(date1.equals(date2)); //output: true

// compare using the compareTo() method
// this method returns 0 if the dates are equal, a positive integer if date1 is after date2,
// and a negative integer if date1 is before date2
System.out.println(date1.compareTo(date2)); //output: 0 (equal)

// compare using the before() and after() methods
// these methods return a boolean value indicating if the date is before or after the specified date
System.out.println(date1.before(date2)); //output: false
System.out.println(date1.after(date2)); //output: false
```

## Deep Dive:
Historically, comparing dates in Java was done using the `Date` class's `getTime()` method, which returned the number of milliseconds since January 1, 1970, at 00:00:00 GMT. This allowed for simple numerical comparisons, but it was not always accurate due to time zone and daylight saving time discrepancies.

With the introduction of the `java.time` package in Java 8, the recommended approach is to use the `LocalDate` class. This class represents a date without time or time zone information, making it more reliable for comparing dates.

Other alternative methods for comparing dates include using the `Calendar` class, which is a bit more complex and has been mostly replaced by the `LocalDate` class, or using a third-party library like Joda-Time.

## See Also:
- [Java API documentation for `Date`](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java API documentation for `LocalDate`](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Joda-Time library](https://www.joda.org/joda-time/)
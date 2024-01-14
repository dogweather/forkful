---
title:    "Java recipe: Comparing two dates"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why 

Comparing dates is a fundamental task in programming, especially in Java. It allows us to determine the chronological order of events and make decisions based on date-related data. Whether you're creating a calendar or organizing historical events, knowing how to compare dates is a valuable skill for any Java programmer.

## How To 

The first step to comparing dates is to understand the `Date` class in Java. This class represents a specific moment in time and holds the date and time information. To compare two dates, we need to create two instances of the `Date` class and then use the `compareTo()` method. Here's an example: 

```Java 
Date date1 = new Date(2020, 8, 26); // August 26, 2020
Date date2 = new Date(2021, 1, 5); // January 5, 2021
int result = date1.compareTo(date2); // result will be -1 (date1 is before date2)
```

The `compareTo()` method returns an integer value that indicates the relative order of the two dates. If the first date is before the second, it returns a negative value. If the first date is after the second, it returns a positive value. Finally, if the dates are equal, it returns 0.

Another approach to comparing dates is by using the `before()` and `after()` methods. These methods return boolean values and can be useful if you want to check multiple conditions. Here's an example: 

```Java 
Date date1 = new Date(1990, 5, 26); // June 26, 1990 
Date date2 = new Date(1995, 1, 5); // January 5, 1995 
if (date1.before(date2)) { 
    System.out.println("Date 1 is before Date 2"); 
} 
if (date2.after(date1)) { 
    System.out.println("Date 2 is after Date 1"); 
} 
```

As you can see, comparing dates in Java is relatively straightforward. However, it's essential to keep in mind that the `Date` class in Java has been deprecated since Java 8. It's recommended to use the `LocalDate` class from the `java.time` package instead, which has more features and is more efficient. Here's an example of comparing dates using the `LocalDate` class: 

```Java 
LocalDate date1 = LocalDate.of(1998, 11, 23); // November 23, 1998 
LocalDate date2 = LocalDate.of(1995, 4, 15); // April 15, 1995 
if (date1.isBefore(date2)) { 
    System.out.println("Date 1 is before Date 2"); 
} 
```

## Deep Dive 

When comparing dates, it's essential to understand that not all dates are equal. There are several factors that we need to consider, such as time zones, daylight savings, and leap years. The `Date` and `LocalDate` classes handle these factors differently, so it's crucial to use the correct class for your needs. 

Additionally, comparing dates with just the `compareTo()` method may not provide accurate results. It only compares the dates' numerical values, not taking into account the time. If you need to compare the time as well, you can use the `isEqual()` method from the `LocalDateTime` class. 

## See Also 

- [Java Date and Time API Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) 
- [Understanding Date and Time in Java](https://www.baeldung.com/java-date-time) 
- [Java Deprecated Date Class](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
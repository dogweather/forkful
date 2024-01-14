---
title:                "Java recipe: Calculating a date in the future or past"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why 

As programmers, we often encounter situations where we need to calculate a date in the future or past. This could be for tasks such as event scheduling, data analysis, or even creating reminders. In Java, we have powerful built-in libraries that make date calculations easy and efficient. In this blog post, we will explore the different methods and techniques that can be used to calculate dates in the future or past.

## How To

To begin, let's take a look at the various ways in which we can perform date calculations in Java.

```
Java.time.LocalDate currentDate = java.time.LocalDate.now();
Java.time.LocalDate futureDate = currentDate.plusDays(10); 
System.out.println("The date 10 days from now is: " + futureDate);
```

In the code above, we used the `plusDays()` method from the `LocalDate` class to add 10 days to the current date. This method is one of the many options available in the `java.time` library for manipulating dates.

We can also use the `Calendar` class to perform date calculations. 

```
Java.util.Calendar currentDate = java.util.Calendar.getInstance();
currentDate.add(Calendar.MONTH, 3);
System.out.println("The date 3 months from now is: " + currentDate.getTime());
```

In this example, we used the `add()` method from the `Calendar` class to add 3 months to the current date. The `getTime()` method is used to convert the `Calendar` object to a `Date` object so that it can be printed.

Calculating dates in the past follows a similar approach. We can use the `minus` methods from the `java.time` library or `add()` methods with negative values from the `Calendar` class.

```
Java.time.LocalDate pastDate = currentDate.minusDays(5);
Java.time.LocalDate pastDate2 = currentDate.minus(1, java.time.temporal.ChronoUnit.WEEKS);
System.out.println("The date 5 days before is: " + pastDate);
System.out.println("The date 1 week before is: " + pastDate2);
```

In the code snippet above, we have demonstrated two ways to calculate a date in the past using the `LocalDate` class.

## Deep Dive

Behind the scenes, Java stores dates as a `long` value representing the number of milliseconds since January 1, 1970, UTC. This is known as the epoch date. When we perform date calculations, these values are manipulated to arrive at the desired date.

It is important to note that dates in different time zones may vary. In order to perform accurate date calculations, it is recommended to use the `TimeZone` class to specify the desired time zone.

## See Also

- [Java SE 8 Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java Calendar Class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Introduction to Java Time API](https://www.baeldung.com/java-time-api)
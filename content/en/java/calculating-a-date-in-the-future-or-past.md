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

## What & Why?

Calculating a date in the future or past refers to the process of manipulating dates and time to find a specific future or past date. This is commonly done by programmers to perform tasks such as scheduling events, calculating time differences, or creating countdown timers.

## How to:

To calculate a date in the future or past, we can use the built-in ```java.time.LocalDate``` class in Java. Here's an example of finding the date 30 days from today:

```java
LocalDate today = LocalDate.now();
LocalDate futureDate = today.plusDays(30);
System.out.println("30 days from today is: " + futureDate);
```

The output of this code would be: ```30 days from today is: {current_date + 30 days}```.

In a similar manner, we can also calculate a date in the past by using the ```minusDays()``` method. Here's an example that finds the date 1 year ago from today's date:

```java
LocalDate today = LocalDate.now();
LocalDate pastDate = today.minusYears(1);
System.out.println("1 year ago from today was: " + pastDate);
```

The output of this code would be: ```1 year ago from today was: {current_date - 1 year}```.

## Deep Dive

The concept of calculating dates in the future or past goes back to the origin of calendars. In the past, different civilizations used various methods for keeping track of time, but it wasn't until the introduction of the Gregorian calendar in 1582 that we had a standardized system for calculating dates.

In Java, there are also alternative methods for manipulating dates, such as the ```java.util.Calendar``` class. However, it is recommended to use the newer ```java.time.LocalDate``` class, which provides more efficient and easier-to-use methods for calculating dates.

When using the ```plusDays()``` and ```minusDays()``` methods, it's important to note that they return a new instance of the ```LocalDate``` class, and the original date object remains unchanged. This is known as immutability and is a core concept in Java.

## See Also

To learn more about using the ```java.time.LocalDate``` class, check out the official Java documentation here: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html

For a deeper dive into the history of calendars and timekeeping, you can read this interesting article: https://www.timeanddate.com/calendar/intercalation.html
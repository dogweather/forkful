---
title:                "Java recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may wonder why getting the current date is necessary. Well, the current date is often used in applications for various purposes, such as logging, time stamping, and scheduling tasks. It helps to keep track of when a certain event occurred or when a task needs to be performed. So, getting the current date is important for the proper functioning of many programs.

## How To

To get the current date in Java, we can use the built-in `java.util.Date` class or the newer `java.time` API, introduced in Java 8. Let's take a look at some code examples to see how to get the current date using both methods.

```Java
// Using java.util.Date class
import java.util.Date;

// creating a Date object
Date currentDate = new Date();

// printing the current date
System.out.println(currentDate);
```

Output:
```
Tue Aug 03 15:26:56 UTC 2021
```

```Java
// Using java.time API
import java.time.LocalDate;

// getting the current date
LocalDate currentDate = LocalDate.now();

// printing the current date
System.out.println(currentDate);
```

Output:
```
2021-08-03
```

In both examples, we first import the necessary classes, then create an object or call a method to get the current date, and finally print it to the console. As you can see, the output format may differ slightly between the two methods. The `java.util.Date` class provides more details such as time and time zone, while the `java.time.LocalDate` only gives the date in a simple format.

## Deep Dive

Internally, the `java.util.Date` class stores the number of milliseconds since January 1, 1970, 00:00:00 GMT in a 64-bit long value. This value is known as the Unix Epoch, and is often used as a reference point for time calculations. When we create a `Date` object using the default constructor, it sets the value to the current date and time. However, it is important to note that the `Date` class is not recommended for use in newer applications due to various issues, such as not being thread-safe.

On the other hand, the `java.time` API is a more modern and recommended way of handling date and time operations in Java. It offers a variety of classes for specific use cases, such as `LocalDate` for representing a date without time and time zone, `LocalDateTime` for representing a date with time but without time zone, and `ZonedDateTime` for representing a date with time and time zone. It also provides methods for easy manipulation and formatting of date and time values.

## See Also

- [Java Date class documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java Time API documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
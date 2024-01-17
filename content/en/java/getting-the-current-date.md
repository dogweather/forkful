---
title:                "Getting the current date"
html_title:           "Java recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date is an important aspect of programming as it allows developers to record when certain events or processes occur in their code. This information is useful for debugging, creating time-sensitive applications, or simply for keeping track of the passing of time in a program.

## How to:
To get the current date in Java, we can use the `java.util.Date` class. Here is a simple example:
```Java
// Import the Date class
import java.util.Date;  

// Create a Date object
Date currentDate = new Date(); 

// Print the current date
System.out.println("Current date: " + currentDate); 
```
Output:
```
Current date: Mon Oct 04 12:48:38 EDT 2021
```
We can also use the `SimpleDateFormat` class to format the date output in a specific way. For example, if we wanted to display the current date in the format `MM/dd/yyyy`, we can do the following:
```Java
// Import the classes
import java.util.Date;
import java.text.SimpleDateFormat;

// Create a Date object
Date currentDate = new Date();

// Define the format we want
SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");

// Format the date and print it
String formattedDate = dateFormat.format(currentDate);
System.out.println("Current formatted date: " + formattedDate);
```
Output:
```
Current formatted date: 10/04/2021
```

## Deep Dive:
Before Java 8, the `java.util.Date` class was the primary way to get the current date in Java. However, it had some limitations such as not being thread-safe and not providing an easy way to format the date. In Java 8, the `java.time` package was introduced which provides a more robust and user-friendly way to handle date and time. The `LocalDate` class in this package can be used to get the current date. Here is an example:
```Java
// Import the LocalDate class
import java.time.LocalDate;

// Create a LocalDate object with the current date
LocalDate currentDate = LocalDate.now();

// Print the current date
System.out.println("Current date: " + currentDate);
```
Output:
```
Current date: 2021-10-04
```

In addition to the `LocalDate` class, the `java.time` package also offers other classes such as `LocalDateTime` and `ZonedDateTime` for handling time zones and more precise dates and times. It is recommended to use these classes for getting the current date in Java instead of the `java.util.Date` class.

## See Also:
- [Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
- [Java Date and Time Tutorial](https://www.javatpoint.com/java-date-and-time)
- [java.time package documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
---
title:                "Java recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a date into a string while coding in Java? Maybe you need to display a date in a specific format or save it as a string in a database. Whatever the reason may be, knowing how to convert a date into a string in Java is a useful skill to have in your programming toolbox.

## How To

Converting a date into a string in Java can be done using the `SimpleDateFormat` class. Here's an example of how to do it:

```Java
// Import the necessary libraries
import java.text.SimpleDateFormat;
import java.util.Date;

// Create a new SimpleDateFormat object with the desired date format
SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");

// Create a Date object with the current date and time
Date currentDate = new Date();

// Use the SimpleDateFormat object to format the date into a string
String dateString = dateFormat.format(currentDate);

// Print the result
System.out.println(dateString);
```

The output of this code will be the current date in the format of "MM/dd/yyyy". You can change the format by using different patterns in the `SimpleDateFormat` constructor. You can also use this class to parse a string into a date object.

## Deep Dive

In addition to the standard patterns like "MM/dd/yyyy", `SimpleDateFormat` also has more customizable options such as using single letters to represent the month ("M") or day of the week ("E"). You can also use a combination of letters and symbols to create a unique format.

It's important to note that `SimpleDateFormat` is not thread-safe, meaning it can cause errors if used in a multi-threaded environment. An alternative is to use the `DateTimeFormatter` class from the Java 8 Date/Time API, which is thread-safe.

## See Also

- [Oracle Documentation for SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java 8 DateTimeFormatter Tutorial](https://www.baeldung.com/java-8-date-time-intro)
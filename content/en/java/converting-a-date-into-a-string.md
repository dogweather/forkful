---
title:                "Converting a date into a string"
html_title:           "Java recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string involves converting a date object into a human-readable string representation. This is useful because it allows programmers to display dates in various formats, depending on the needs of the application or user. It also makes it easier to work with dates in different programming languages and systems.

## How to:
To convert a date into a string in Java, we can use the `SimpleDateFormat` class. We first create an instance of this class, passing in the desired date format as a string. Then, we can use the `format()` method to convert the date object into a string in the specified format.

```Java
SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
Date date = new Date();
String dateString = dateFormat.format(date);

System.out.println(dateString); // Output: 03/17/2021
```

We can also include time in our string representation by adding hours, minutes, and seconds to the format.

```Java
SimpleDateFormat dateTimeFormat = new SimpleDateFormat("MM/dd/yyyy hh:mm:ss");
Date date = new Date();
String dateTimeString = dateTimeFormat.format(date);

System.out.println(dateTimeString); // Output: 03/17/2021 09:23:15
```

## Deep Dive
The `SimpleDateFormat` class was introduced in Java 1.1 and is part of the `java.text` package. It supports a variety of date and time formatting options, including different languages and locales. It also provides methods for parsing string representations of dates into date objects.

An alternative to using the `SimpleDateFormat` class is the `DateTimeFormatter` class, introduced in Java 8. This class offers a more modern and thread-safe way to format and parse dates. It also supports new date and time classes introduced in Java 8, such as `LocalDate` and `LocalDateTime`.

Under the hood, the `SimpleDateFormat` class uses a `Calendar` object to handle date and time calculations. It also uses a `Locale` object to determine the language and country for the date representation. These components can be accessed and modified through the `setCalendar()` and `setLocale()` methods of the `SimpleDateFormat` class.

## See Also:
- [Official Java Documentation for SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Official Java Documentation for DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Stack Overflow: Oracle recommends avoiding SimpleDateFormat and switching to DateTimeFormatter](https://stackoverflow.com/questions/12035482/simpledateformat-thread-safety/37213843#37213843)
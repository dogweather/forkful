---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Turning a date into a string in Java means going from the DateTime data type, into something legible for humans—an operation we call formatting. We do this so end-users can read, use or save date data in a user-friendly format.

## How to:
Here's an example of how to convert a Date to a String in Java using SimpleDateFormat:

```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
  public static void main(String[] args) {
    Date currentDate = new Date(); // This gets the current date-time.
    SimpleDateFormat format = new SimpleDateFormat("MM-dd-yyyy HH:mm:ss"); // This sets the format.
    String stringDate = format.format(currentDate); // This converts to string.
    System.out.println(stringDate);  // This prints the date.
  }
}
```

When you run this script, you'll see something like "05-08-2022 19:01:30" printed on your console—simple, fast, and useful.

## Deep Dive
Formatting dates have been necessary since the inception of programming, as it's key to interface with end-users. In Java, the DateFormat class, introduced in JDK 1.1, initially allowed this. But DateFormat had thread-safety and performance issues, leading to the more robust SimpleDateFormat in Java 1.2.

Alternatives to SimpleDateFormat exist: Java 8 introduced DateTimeFormatter, which is not only thread-safe but also immutables (a win!). If you're dealing with older codebases or libraries still hung up on java.util.Calendar and java.util.Date, Joda-Time library poses an excellent option.

Implementing these conversions is routine—just remember, dates are managed as milliseconds betwixt Jan 1, 1970, and the date in question. The crux is parsing these milliseconds into a user-friendly format.

## See Also
- Oracle Docs: [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html), [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Baeldung: Converting between Date, Calendar, and LocalDate](https://www.baeldung.com/java-convert-date-to-localdate-and-back)
- [Princeton Java: Dates, times and intervals](https://introcs.cs.princeton.edu/java/stdlib/javadoc/edu/princeton/cs/stdlib/Date.html)
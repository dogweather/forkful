---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date in Java is all about fetching real-time date information while the program is running. Whether you're logging events, timestamping user activities, or simply clock displays, you need to do it right.

## How to:

Here's a quick example that employs `java.time.LocalDate` which is part of Java 8 and onwards:

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now();
        System.out.println("Today's Date: " + date);
    }
}
```
When you run this program, it outputs the current date in the format `yyyy-mm-dd`. For instance, you might get:

```
Today's Date: 2022-06-10
```

## Deep Dive:

* **Historical Context**: Back in the day, Java developers used `java.util.Date` to get the current date. Java 8 introduced a new date and time API under `java.time` package providing a simpler, cleaner, and more accurate solution.

* **Alternatives**: If you need more detailed time information, use `java.time.LocalDateTime` or `java.time.ZonedDateTime` for time zone-specific data. For instance:

```Java
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        LocalDateTime dateTime = LocalDateTime.now();
        System.out.println("Current Date and Time: " + dateTime);
    }
}
```

* **Implementation Details**: `LocalDate.now()` internally uses `Clock.systemDefaultZone()` method, which obtains a clock that returns current instant using best available system clock, converting to date using the default time-zone.

## See Also:

* [Oracle Java Docs: LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
* [Oracle Java Docs: LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
* [Oracle tutorial on New Date and Time](https://docs.oracle.com/javase/tutorial/datetime/iso/overview.html)
* [Baeldung Guide on New Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
---
title:                "Getting the current date"
date:                  2024-01-20T15:14:56.897186-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Grabbing the current date in Java is as easy as pie – it's about using the right class to snatch the present day from the system clock. Programmers do this for tracking, logging, or to pepper their apps with time-sensitive features.

## How to:

### Fetch Today's Date

```java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        System.out.println("Today's Date: " + today);
    }
}
```

**Sample Output:**
```
Today's Date: 2023-04-01
```

### Time with More Details

```java
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        LocalDateTime now = LocalDateTime.now();
        System.out.println("Current Date and Time: " + now);
    }
}
```

**Sample Output:**
```
Current Date and Time: 2023-04-01T12:45:30.123
```

## Deep Dive:

Before Java 8, `java.util.Date` and `java.util.Calendar` were the go-tos for date-time. But they were clunky and non-intuitive. Java 8 introduced `java.time`, a more robust and understandable API. `java.time.LocalDate` fetches the date without time, while `java.time.LocalDateTime` gets date and time, without a time zone. If you need the time zone, there's `java.time.ZonedDateTime`. For just time, there's `java.time.LocalTime`.

As for alternatives, libraries like Joda-Time existed pre-Java 8, and some legacy projects might still use it. But since Java 8's introduction of the `java.time` package, it's been considered the standard, and for good reasons. It’s comprehensive and aligns with the ISO-8601 calendar system.

From an implementation perspective, `now()` methods within `java.time` classes grab the current date/time from the system clock, which is the computer's conception of current time, linked to the real world through system settings and internet time synchronization.

## See Also:

- The official `java.time` package docs: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- ISO 8601 Date and Time Standards: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
- For old-school Java date management, check out the `Calendar` class: [https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html)

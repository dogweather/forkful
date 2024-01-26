---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:36:31.928730-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means converting written text into a Date object that a program can use. Programmers do it to make sense of user input or data stored in readable formats.

## How to:

Java's got a class `java.time.format.DateTimeFormatter` for this kind of job. Here's how you hang with it.

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class DateParser {

    public static void main(String[] args) {
        String dateString = "2023-03-15";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        try {
            LocalDate date = LocalDate.parse(dateString, formatter);
            System.out.println("Parsed date is: " + date);
        } catch (DateTimeParseException e) {
            System.err.println("Oops, date was in the wrong format!");
        }
    }
}
```

Run this little number, and you'll see:

```
Parsed date is: 2023-03-15
```

## Deep Dive

Long before `java.time` swaggered in with Java 8 in 2014, folks wrangled with `java.util.Date` and `SimpleDateFormat`. These old-timers are not only thread-hostile but also a headache to use with their timezone quirks.

Nowadays, `java.time` is the hotness. It's thread-safe, immutable (no sneaky changes), and clearer in intent. Plus, you can pick and choose from a set of predefined formatters or roll your own with patterns.

Alternatives, you ask? Libraries like Joda-Time paved the way, but since java.time borrowed heavily from its ideas, most have hung up their hats in favor of the standard library.

Under the hood, parsing with `DateTimeFormatter` does some heavy lifting. It checks patterns, validates input, handles exceptions, and delivers a shiny `LocalDate`, `LocalTime`, or even `ZonedDateTime` depending on what you're after.

## See Also

- The official Java docs for `java.time.format.DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Oracle's Java tutorials, including date and time: https://docs.oracle.com/javase/tutorial/datetime/

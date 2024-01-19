---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates in programming means assessing their chronological order or equality. This task matters because, for instance, it helps calculate time intervals or sort events.

## How to:

Here's a quick way to compare two dates. Let's use `java.time.LocalDate`, a class in Java's in-built `java.time` package.

```Java
import java.time.LocalDate;

public class MainClass {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2022, 1, 1);
        LocalDate date2 = LocalDate.of(2023, 2, 2);

        if (date1.isBefore(date2)) {
            System.out.println("date1 is before date2");
        } else if (date1.isAfter(date2)) {
            System.out.println("date1 is after date2");
        } else {
            System.out.println("date1 is equal to date2");
        }
    }
}
```

If you run this code, you'll see the output: `date1 is before date2`.

## Deep Dive

**Historical Context**

Early Java versions relied on `java.util.Date`. However, it was mutable (causing many problems) and had poor API design. Since Java 8, we use the `java.time` package, which is more versatile and safer.

**Alternatives**

A popular alternative to Java's in-built date/time library is Joda-Time. But post Java 8, it's recommended to use `java.time` due to its superior features and design.

**Implementation Details**

When two `LocalDate` instances are compared using `isBefore()`, `isAfter()`, or `isEqual(),` their year, month, and day values are compared.

## See Also

1. [Oracle's java.time Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. [Joda-Time library](https://www.joda.org/joda-time/)
3. [Baeldung Guide](https://www.baeldung.com/java-8-date-time-intro) for an overview on Java 8's Date and Time API.
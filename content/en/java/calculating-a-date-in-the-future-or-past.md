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

Calculating a future or past date involves using computing logic to figure out a date from a particular date, adding or subtracting specific time units. Program It's crucial for tasks like setting expiry dates, scheduling events, or estimating project deadlines.

## How To:

In Java, you can use the `LocalDate` class from the `java.time` package. Look at this sample code:

```Java
import java.time.LocalDate;
import java.time.Period;

public class FutureDate {
  public static void main(String[] args) {
    // current date
    LocalDate currentDate = LocalDate.now();
    System.out.println("Current Date: " + currentDate);

    // adding one month
    LocalDate futureDate = currentDate.plus(Period.ofMonths(1));
    System.out.println("Future Date: " + futureDate);
  }
}
```

Output:

```Java
Current Date: 2022-03-29
Future Date: 2022-04-29
```

In the code above, we used `plus()` method to add to the current date and `ofMonths()` to specify the period unit, in this case, a month. Replace `ofMonths()` with `ofDays()` or `ofYears()` as needed.

## Deep Dive

Historically, date and time computations were complex in Java. The `java.util.Date` and `java.util.Calendar` classes were bulky and error-prone. Introduced in Java 8, the `java.time` package simplified date-time computations.

Alternatives to `LocalDate` include the older `java.util.Date` and `java.util.Calendar` classes. There are also external libraries like Joda-Time. However, `LocalDate` is easier and more efficient. 

`LocalDate` is immutable (thread-safe) and follows ISO 8601 standard (YYYY-MM-DD format). The `plus()` and `minus()` methods are used for date computations. If any computation causes a date overflow or underflow, these methods will throw a `DateTimeException`.

## See Also

For more details, refer to the following official Java documentations:

- [LocalDate (Java SE 11 & JDK 11 )](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Period (Java SE 11 & JDK 11 )](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Period.html)
  
For historical context and alternatives, visit:

- [The Legacy Date-Time API (The Java™ Tutorials)](https://docs.oracle.com/javase/tutorial/datetime/iso/legacy.html)
- [Joda-Time - Home](http://www.joda.org/joda-time/)
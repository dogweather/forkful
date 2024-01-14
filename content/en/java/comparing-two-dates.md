---
title:                "Java recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to compare two dates in a Java program? Maybe you want to check if a certain event is happening before or after another. Or perhaps you need to calculate the time difference between two dates. Whatever your reason may be, learning how to compare dates in Java can come in handy in many situations.

## How To 

To compare dates in Java, we will be using the `java.util.Date` class. This class represents a specific moment in time and has methods to perform various operations on dates. Here is an example of comparing two dates using the `before()` and `after()` methods:

```Java
import java.util.Date;

public class DateComparisonExample {

    public static void main(String[] args) {
        // Create two sample dates
        Date date1 = new Date();
        Date date2 = new Date(2020, 7, 23); // Year, month, day

        // Compare dates
        if (date1.before(date2)) {
            System.out.println("Date 1 is before Date 2");
        } else {
            System.out.println("Date 2 is before Date 1");
        }

        if (date1.after(date2)) {
            System.out.println("Date 1 is after Date 2");
        } else {
            System.out.println("Date 2 is after Date 1");
        }
    }
}
```

Output:
```
Date 1 is after Date 2
Date 2 is before Date 1
```

You can also use the `compareTo()` method to compare two dates. This method returns an integer value based on the comparison result, with a positive value indicating Date 1 is after Date 2, a negative value indicating Date 1 is before Date 2, and 0 indicating both dates are equal. Here is an example:

```Java
import java.util.Date;

public class DateComparisonExample {

    public static void main(String[] args) {
        // Create two sample dates
        Date date1 = new Date();
        Date date2 = new Date(2020, 7, 23); // Year, month, day

        // Compare dates
        int result = date1.compareTo(date2);
        if (result > 0) {
            System.out.println("Date 1 is after Date 2");
        } else if (result < 0) {
            System.out.println("Date 1 is before Date 2");
        } else {
            System.out.println("Both dates are equal");
        }
    }
}
```

Output: 
```
Date 1 is after Date 2
```

## Deep Dive 

While comparing two dates may seem like a simple task, it is important to understand that when using the `Date` class, the comparison is based on the millisecond values of the dates. This means that the result of the comparison can be affected by factors like time zones, leap seconds, and daylight saving time. 

To avoid these issues, it is recommended to use the newer `java.time` package introduced in Java 8. This package provides the `LocalDate` and `LocalDateTime` classes which can be used to compare dates without any time zone or daylight saving time complications. Here is an example:

```Java
import java.time.LocalDate;

public class DateComparisonExample {

    public static void main(String[] args) {
        // Create two sample dates
        LocalDate date1 = LocalDate.now();
        LocalDate date2 = LocalDate.of(2020, 7, 23); // Year, month, day

        // Compare dates
        if (date1.isBefore(date2)) {
            System.out.println("Date 1 is before Date 2");
        } else {
            System.out.println("Date 2 is before Date 1");
        }

        if (date1.isAfter(date2)) {
            System.out.println("Date 1 is after Date 2");
        } else {
            System.out.println("Date 2 is after Date 1");
        }
    }
}
```

Output:
```
Date 1 is after Date 2
Date 2 is before Date 1
```

## See Also 

- [Comparing Dates in Java](https://www.baeldung.com/java-compare-dates)
- [Comparison of dates in Java](https://www.javatpoint.com/compare-dates-in-java)
- [Date and Time Tutorial](https://docs.oracle.com/javase/tutorial/datetime/index.html)
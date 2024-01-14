---
title:    "Java recipe: Getting the current date"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why
In the world of programming, we are often required to manipulate time and dates. Whether it's for displaying information, creating schedules, or tracking data, having accurate and up-to-date information is crucial. Luckily, Java provides us with a simple and efficient way to obtain the current date and time. In this blog post, we will explore why getting the current date is important and how to do it in Java.

## How To
First, we need to import the `java.time` package to access the date and time classes in Java. Then, we can use the `LocalDate` class to get the current date. Let's take a look at the code below:

```java
import java.time.LocalDate;

public class CurrentDate {
    public static void main(String[] args) {
        // get current date
        LocalDate currentDate = LocalDate.now();
        System.out.println("Today's Date: " + currentDate);
    }
}
```

Here, we use the `LocalDate.now()` method to get the current date and assign it to the `currentDate` variable. We then use `System.out.println()` to print out the result, which will give us the current date in the format of `Year-Month-Day`.

We can also get the current time by using the `LocalTime` class in a similar way. Let's see an example:

```java
import java.time.LocalTime;

public class CurrentTime {
    public static void main(String[] args) {
        // get current time
        LocalTime currentTime = LocalTime.now();
        System.out.println("Current Time: " + currentTime);
    }
}
```

This will print out the current time in the format of `Hour:Minute:Second`.

## Deep Dive
Now that we know how to get the current date and time, let's take a deeper look at the `LocalDate` class. This class provides us with various methods to manipulate and format the date. We can also use the `DateTimeFormatter` class to customize the output format. Here's an example:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class FormattedDate {
    public static void main(String[] args) {
        // get current date
        LocalDate currentDate = LocalDate.now();

        // format date using DateTimeFormatter
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String formattedDate = currentDate.format(formatter);

        System.out.println("Formatted Date: " + formattedDate);
    }
}
```

In this code, we use the `DateTimeFormatter` class to specify the format we want the date to be in, which is `dd/MM/yyyy` (day/month/year). Then, we use the `format()` method to format the `currentDate` and assign it to the `formattedDate` variable.

## See Also
- <https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html>
- <https://docs.oracle.com/javase/8/docs/api/java/time/LocalTime.html>
- <https://www.baeldung.com/java-date-time-api>
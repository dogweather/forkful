---
title:    "Java recipe: Calculating a date in the future or past"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why 

As developers, we often encounter situations where we need to calculate dates in the future or past. This could be for tasks like scheduling, setting reminders, or simply keeping track of time. Knowing how to calculate dates in Java can save us time and effort in such scenarios.

## How To 

Calculating dates in Java is made easy with the built-in `Calendar` and `SimpleDateFormat` classes. Here's a simple example of how to calculate a date 5 days from today:

```
// Import necessary libraries
import java.util.Calendar;
import java.text.SimpleDateFormat;
import java.util.Date;

// Create a Calendar instance and set it to today's date
Calendar calendar = Calendar.getInstance();
// Add 5 days to the current date
calendar.add(Calendar.DAY_OF_MONTH, 5);
// Use a SimpleDateFormat to format the date in the desired format
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
// Convert the date to a string and print it
System.out.println(dateFormat.format(calendar.getTime()));
```

The output of this code will be: `29/06/2021`, as today's date is `24/06/2021` and we added 5 days to it.

We can also calculate dates in the past by using a negative value in the `add` method. For example, to calculate 2 weeks ago:

```
// Create a Calendar instance and set it to today's date
Calendar calendar = Calendar.getInstance();
// Subtract 2 weeks from the current date
calendar.add(Calendar.WEEK_OF_MONTH, -2);
// Use a SimpleDateFormat to format the date in the desired format
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
// Convert the date to a string and print it
System.out.println(dateFormat.format(calendar.getTime()));
```

The output of this code will be: `10/06/2021`.

## Deep Dive 

The `Calendar` class in Java allows us to perform various operations on dates, including addition and subtraction. The `Calendar` class uses a `Locale` to determine the language, country, and culture when formatting dates. It's important to note that the `Calendar` class is not thread-safe, so it's recommended to use the newer `LocalDate` and `LocalDateTime` classes for multithreaded environments.

When using the `SimpleDateFormat` class, we can specify the format for our date using patterns such as `dd/MM/yyyy` or `MM/dd/yyyy`. It's also possible to customize the `Locale` for date formatting, as well as specify time zones and handle conversions between different time zones.

## See Also 

- [Official Java Documentation for Calendar Class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Official Java Documentation for LocalDate Class](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Official Java Documentation for LocalDateTime Class](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
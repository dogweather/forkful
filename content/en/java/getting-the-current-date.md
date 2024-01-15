---
title:                "Getting the current date"
html_title:           "Java recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Getting the current date is a common task in programming, whether it be for tracking events, scheduling tasks, or simply displaying the current date and time in an application. Java offers an efficient and reliable way to obtain the current date, making it a useful skill for any Java developer.

## How To
To get the current date in Java, we can use the `LocalDate` class from the `java.time` package. First, we need to import the class into our program using the `import` statement:

```Java
import java.time.LocalDate;
```

Next, we can use the `now()` method of the `LocalDate` class to create an instance of `LocalDate` representing the current date:

```Java
LocalDate currentDate = LocalDate.now();
```

We can then use the `toString()` method to convert the `LocalDate` object into a string, and print it out to see the current date:

```Java
System.out.println(currentDate.toString());
```

This will output the current date in the format of `YYYY-MM-DD`, such as `2021-07-13`.

If we want to get the current date and time, we can use the `LocalDateTime` class instead, which also belongs to the `java.time` package. The steps are similar, but we will use the `now()` method of the `LocalDateTime` class this time:

```Java
import java.time.LocalDateTime;

LocalDateTime currentDateTime = LocalDateTime.now();
System.out.println(currentDateTime.toString());
```

This will output the current date and time in the format of `YYYY-MM-DDThh:mm:ss`, such as `2021-07-13T13:45:29`.

## Deep Dive
Behind the scenes, the `now()` method of both `LocalDate` and `LocalDateTime` classes is utilizing the `Clock` class to obtain the current time in the system's default time zone. The `Clock` class also offers the option to specify a different time zone or even a different source of time, such as an atomic clock from a time server.

Additionally, the `now()` method can take in an `ZoneId` as an argument, which allows us to get the current date and time in a specific time zone. For example, to get the current date and time in the Eastern Time Zone, we can use:

```Java
import java.time.LocalDateTime;
import java.time.ZoneId;

LocalDateTime currentDateTime = LocalDateTime.now(ZoneId.of("US/Eastern"));
System.out.println(currentDateTime.toString());
```

This will output the current date and time based on the Eastern Time Zone, such as `2021-07-13T14:45:29`.

## See Also
- [Official Java Documentation - LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Official Java Documentation - LocalDateTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDateTime.html)
- [Official Java Tutorial - Date and Time](https://docs.oracle.com/javase/tutorial/datetime/index.html)
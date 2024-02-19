---
aliases:
- /en/java/getting-the-current-date/
date: 2024-02-03 19:03:03.288829-07:00
description: "Obtaining the current date in Java is a fundamental operation that allows\
  \ programmers to manipulate date objects for operations such as logging, date\u2026"
lastmod: 2024-02-18 23:09:10.941037
model: gpt-4-0125-preview
summary: "Obtaining the current date in Java is a fundamental operation that allows\
  \ programmers to manipulate date objects for operations such as logging, date\u2026"
title: Getting the current date
---

{{< edit_this_page >}}

## What & Why?
Obtaining the current date in Java is a fundamental operation that allows programmers to manipulate date objects for operations such as logging, date calculations, and time-based conditions. It's vital in applications where tracking, scheduling, and temporal data analysis are crucial.

## How to:
Java offers multiple ways to get the current date, using both the old `java.util.Date` class and the newer `java.time` package (introduced in Java 8) which is more versatile and intuitive.

### Using `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Example output: 2023-04-01
    }
}
```
### Using `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Example output: 2023-04-01T12:34:56.789
    }
}
```
### Using `java.util.Date` (Legacy)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Example output: Sat Apr 01 12:34:56 BST 2023
    }
}
```
### Utilizing a Third-party Library: Joda-Time
Before Java 8, Joda-Time was the de-facto standard for date and time in Java. If you are working on legacy systems or have a preference for Joda-Time, here’s how you can use it to get the current date:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Example output: 2023-04-01
    }
}
```
**Note:** While `java.util.Date` and Joda-Time are still used, the `java.time` package is recommended for new projects due to its immutability and comprehensive API for handling dates and times.

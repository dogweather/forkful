---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of converting written text in specific date format into a date object that the machine can understand. We do this to allow applications to present dates conveniently and process them efficiently.

## How To:

Here's a simple way to parse a date from a string in Java using the `LocalDate` class.

```java 
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        String stringDate = "2022-06-09";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate parsedDate = LocalDate.parse(stringDate, formatter);
        System.out.println("Parsed date: " + parsedDate);
    }
}
```

When you run this code, you should see:
```
Parsed date: 2022-06-09
```

## Deep Dive

Historically, the `java.util.Date` and `java.text.SimpleDateFormat` classes were used to parse dates, but these are largely outdated and have been replaced by the `java.time.LocalDate` class in Java 8+ due to its simplicity and improved capabilities. 

The key to parsing a date from a string is proper formatting. The `DateTimeFormatter` lets you specify the expected format of the string you're parsing. This allows for a wider variety of date formats to be handled.

Alternative methods exist, each with its pros and cons. `java.text.SimpleDateFormat` is more flexible and has wider implementation, but it's not thread-safe. `java.text.DateFormat` is thread-safe but lacks the flexibility of `SimpleDateFormat`. Lastly, `java.time.LocalDate` is both thread-safe and flexible, but only available in Java 8 and later.

## See Also

For additional reading and resources, check out:
 
1. The official Oracle documentation on [java.time.LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
2. The Stack Overflow post on ["Parse date string to some Java object"](https://stackoverflow.com/questions/4024544/how-to-parse-date-string-to-some-java-object)
3. A comprehensive guide on ["Working with the Java Date and Time API"](https://www.baeldung.com/java-8-date-time-intro)
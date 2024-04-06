---
date: 2024-01-20 17:36:55.748956-07:00
description: "How to: (\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) Converting\
  \ dates to strings isn't new. With Java's development, historic classes like `java.util.Date`\
  \ were superseded by the\u2026"
lastmod: '2024-04-05T22:51:02.211177-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) Converting dates\
  \ to strings isn't new."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## How to: (Як зробити:)
```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        // Створення екземпляру LocalDateTime
        LocalDateTime now = LocalDateTime.now();

        // Форматування дати у строку
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        String formattedDate = now.format(formatter);

        // Вивід строки
        System.out.println(formattedDate);
    }
}
```

Sample Output:

```
2023-03-15 12:45:30
```

## Deep Dive (Глибоке Занурення)
Converting dates to strings isn't new. With Java's development, historic classes like `java.util.Date` were superseded by the `java.time` package from Java 8 for better time-zone handling and immutability. Alternatives? Sure. You've got `SimpleDateFormat` (now less used due to thread safety issues), third-party libraries like Joda-Time (inspiration for `java.time`), and database-specific formats (watch out for SQL injection!). Implementation wise, `DateTimeFormatter` is king now, with its thread-safe, immutable objects.

## See Also (Дивіться також)
For more, hover through the Java API docs and some well-versed articles:
- Official Java documentation on `LocalDateTime`: https://docs.oracle.com/javase/10/docs/api/java/time/LocalDateTime.html
- Oracle tutorial on `DateTimeFormatter`: https://docs.oracle.com/javase/tutorial/datetime/iso/format.html
- More about thread safety and date-time classes: https://www.baeldung.com/java-thread-safety
- If curious about Joda-Time: https://www.joda.org/joda-time/

---
date: 2024-01-20 17:36:55.748956-07:00
description: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u0443\u0432\u0430\u043D\
  \u043D\u044F \u0434\u0430\u0442\u0438 \u0443 \u0440\u044F\u0434\u043E\u043A \u0434\
  \u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u0437\u0431\u0435\u0440\u0456\u0433\u0430\
  \u0442\u0438 \u0434\u0430\u0442\u0443 \u0432 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u043C\u0443 \u0444\u043E\u0440\u043C\u0430\u0442\u0456, \u044F\u043A\
  \u0438\u0439 \u043B\u0435\u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0442\u0438\
  \ \u0439 \u043E\u0431\u043C\u0456\u043D\u044E\u0432\u0430\u0442\u0438\u0441\u044F\
  \ \u043D\u0438\u043C. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\
  \u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\
  \ \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F,\u2026"
lastmod: 2024-02-19 22:05:08.117899
model: gpt-4-1106-preview
summary: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u0443\u0432\u0430\u043D\
  \u043D\u044F \u0434\u0430\u0442\u0438 \u0443 \u0440\u044F\u0434\u043E\u043A \u0434\
  \u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u0437\u0431\u0435\u0440\u0456\u0433\u0430\
  \u0442\u0438 \u0434\u0430\u0442\u0443 \u0432 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u043C\u0443 \u0444\u043E\u0440\u043C\u0430\u0442\u0456, \u044F\u043A\
  \u0438\u0439 \u043B\u0435\u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0442\u0438\
  \ \u0439 \u043E\u0431\u043C\u0456\u043D\u044E\u0432\u0430\u0442\u0438\u0441\u044F\
  \ \u043D\u0438\u043C. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\
  \u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\
  \ \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F,\u2026"
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Преобразування дати у рядок дозволяє зберігати дату в текстовому форматі, який легко читати й обмінюватися ним. Програмісти роблять це для логування, відображення дат користувачам чи збереження у базах даних.

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

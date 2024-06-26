---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:41.786479-07:00
description: "\u041A\u0430\u043A: Java \u0434\u0435\u043B\u0430\u0435\u0442 \u043F\
  \u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u0434\
  \u0430\u0442\u044B \u0432 \u0441\u0442\u0440\u043E\u043A\u0443 \u043F\u0440\u043E\
  \u0441\u0442\u044B\u043C \u0438 \u043F\u043E\u043D\u044F\u0442\u043D\u044B\u043C\
  . \u041A\u043B\u0430\u0441\u0441 `java.time.format.DateTimeFormatter` \u0431\u0443\
  \u0434\u0435\u0442 \u0432\u0430\u0448\u0438\u043C \u043B\u0443\u0447\u0448\u0438\
  \u043C \u043F\u043E\u043C\u043E\u0449\u043D\u0438\u043A\u043E\u043C. \u0412\u043E\
  \u0442 \u043F\u0440\u0438\u043C\u0435\u0440\u2026"
lastmod: '2024-03-13T22:44:44.843330-06:00'
model: gpt-4-0125-preview
summary: "Java \u0434\u0435\u043B\u0430\u0435\u0442 \u043F\u0440\u0435\u043E\u0431\
  \u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u0434\u0430\u0442\u044B\
  \ \u0432 \u0441\u0442\u0440\u043E\u043A\u0443 \u043F\u0440\u043E\u0441\u0442\u044B\
  \u043C \u0438 \u043F\u043E\u043D\u044F\u0442\u043D\u044B\u043C."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0434\u0430\u0442\u044B \u0432 \u0441\u0442\u0440\u043E\u043A\u0443"
weight: 28
---

## Как:
Java делает преобразование даты в строку простым и понятным. Класс `java.time.format.DateTimeFormatter` будет вашим лучшим помощником. Вот пример кода:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now(); // Сегодняшняя дата
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String dateString = date.format(formatter);
        System.out.println(dateString); // Вывод может быть, например, 20/03/2023
    }
}
```

## Подробнее
Исторически Java использовала `SimpleDateFormat` из пакета `java.text`, но он не был потокобезопасным и приводил к ошибкам. Начиная с Java 8, пакет `java.time` принёс с собой потокобезопасные и неизменяемые классы для работы с датой и временем. `DateTimeFormatter` является частью этого современного пакета.

Существуют альтернативные решения, такие как `FastDateFormat` от Apache Commons и `DateUtils` из различных библиотек. Тем не менее, большинство Java-разработчиков придерживаются стандартной библиотеки, которая является надёжной и универсальной.

При форматировании `DateTimeFormatter` использует шаблоны `yyyy` для года, `MM` для месяца и `dd` для дня. Он может обрабатывать довольно сложные шаблоны, даже специфичные для локали, с использованием метода `ofPattern`. Также стоит отметить, что `DateTimeFormatter` неизменяем и потокобезопасен, поэтому вы можете использовать один и тот же экземпляр форматера в нескольких потоках без головной боли из-за необходимости синхронизации.

## См. также
- Официальная документация Oracle для `DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Для большего количества шаблонов даты и времени: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns
- Обзор даты и времени в Java 8: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html

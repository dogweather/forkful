---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:38.781203-07:00
description: ''
lastmod: '2024-04-05T22:00:00.524394-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
weight: 30
---

## Як:


### Використовуючи пакунок `java.time` (Рекомендовано в Java 8 та пізніше):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Вивід: 2023-04-30
    }
}
```

### Використовуючи `SimpleDateFormat` (Старий підхід):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // Формат виводу залежить від формату за замовчуванням вашої системи
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Використовуючи сторонні бібліотеки (напр., Joda-Time):
Joda-Time була значною сторонньою бібліотекою, але тепер знаходиться в режимі обслуговування через введення пакунка `java.time` в Java 8. Однак для тих, хто використовує версії Java до 8, Joda-Time є гарним вибором.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Вивід: 2023-04-30
    }
}
```
Зверніть увагу, що при роботі з датами завжди будьте обачні щодо налаштувань часового поясу, якщо ви аналізуєте або форматуєте дати-часи, а не просто дати.

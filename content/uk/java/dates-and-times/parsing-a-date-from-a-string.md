---
aliases:
- /uk/java/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:38.781203-07:00
description: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437\
  \ \u0440\u044F\u0434\u043A\u0430 \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u043F\
  \u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\
  \u0441\u0442\u043E\u0432\u043E\u0433\u043E \u043F\u0440\u0435\u0434\u0441\u0442\u0430\
  \u0432\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\u0438 \u0442\u0430 \u0447\
  \u0430\u0441\u0443 \u043D\u0430 \u043E\u0431'\u0454\u043A\u0442 `Date` \u0430\u0431\
  \u043E \u0431\u0456\u043B\u044C\u0448 \u0441\u0443\u0447\u0430\u0441\u043D\u0438\
  \u0439 \u043E\u0431'\u0454\u043A\u0442 `LocalDateTime`. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
lastmod: 2024-02-18 23:09:00.135737
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430 \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u043F\u0435\
  \u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\
  \u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\u0438 \u0442\u0430 \u0447\u0430\
  \u0441\u0443 \u043D\u0430 \u043E\u0431'\u0454\u043A\u0442 `Date` \u0430\u0431\u043E\
  \ \u0431\u0456\u043B\u044C\u0448 \u0441\u0443\u0447\u0430\u0441\u043D\u0438\u0439\
  \ \u043E\u0431'\u0454\u043A\u0442 `LocalDateTime`. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438\u2026"
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## Що і чому?
Розбір дати з рядка включає перетворення текстового представлення дати та часу на об'єкт `Date` або більш сучасний об'єкт `LocalDateTime`. Програмісти роблять це для того, щоб маніпулювати, форматувати, порівнювати або зберігати дати в стандартизованому форматі, що є критично важливим для програм, яким потрібні обчислення дат, їхнє валідування або послідовна інтернаціоналізація.

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

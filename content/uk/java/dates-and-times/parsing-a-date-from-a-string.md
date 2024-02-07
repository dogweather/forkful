---
title:                "Розбір дати з рядка"
date:                  2024-02-03T19:14:38.781203-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

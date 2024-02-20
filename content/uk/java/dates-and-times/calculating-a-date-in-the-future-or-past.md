---
date: 2024-01-20 17:31:28.348302-07:00
description: "\u0420\u043E\u0437\u0440\u0430\u0445\u0443\u043D\u043E\u043A \u0434\u0430\
  \u0442\u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\
  \u0443 \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 \u2014\
  \ \u0446\u0435 \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u043A\
  \u043E\u043D\u043A\u0440\u0435\u0442\u043D\u0438\u0445 \u0434\u043D\u0456\u0432\
  \ \u0432\u0456\u0434 \u0432\u0456\u0434\u043E\u043C\u043E\u0457 \u0434\u0430\u0442\
  \u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\
  \u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u043E\u0431\
  \u0440\u043E\u0431\u043A\u0438 \u0442\u0435\u0440\u043C\u0456\u043D\u0456\u0432\
  , \u043F\u043B\u0430\u043D\u0443\u0432\u0430\u043D\u043D\u044F\u2026"
lastmod: 2024-02-19 22:05:08.120812
model: gpt-4-1106-preview
summary: "\u0420\u043E\u0437\u0440\u0430\u0445\u0443\u043D\u043E\u043A \u0434\u0430\
  \u0442\u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\
  \u0443 \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 \u2014\
  \ \u0446\u0435 \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u043A\
  \u043E\u043D\u043A\u0440\u0435\u0442\u043D\u0438\u0445 \u0434\u043D\u0456\u0432\
  \ \u0432\u0456\u0434 \u0432\u0456\u0434\u043E\u043C\u043E\u0457 \u0434\u0430\u0442\
  \u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\
  \u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u043E\u0431\
  \u0440\u043E\u0431\u043A\u0438 \u0442\u0435\u0440\u043C\u0456\u043D\u0456\u0432\
  , \u043F\u043B\u0430\u043D\u0443\u0432\u0430\u043D\u043D\u044F\u2026"
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
---

{{< edit_this_page >}}

## Що і чому?

Розрахунок дати у майбутньому або минулому — це визначення конкретних днів від відомої дати. Програмісти роблять це для обробки термінів, планування задач, аналізу історичних даних.

## Як це робити:

В Java, для роботи з датами, можна використовувати `LocalDate` та `Period` з пакету `java.time`.

```java
import java.time.LocalDate;
import java.time.Period;

public class DateCalculator {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        Period tenDays = Period.ofDays(10);
        
        LocalDate tenDaysAfter = today.plus(tenDays);
        LocalDate tenDaysBefore = today.minus(tenDays);

        System.out.println("Сьогодні: " + today);
        System.out.println("10 днів після: " + tenDaysAfter);
        System.out.println("10 днів до: " + tenDaysBefore);
    }
}
```

Вищенаведений код виводить сьогоднішню дату, дату через 10 днів та дату 10 днів назад.

## Глибше занурення:

До Java 8, робота з датами була складнішою через `java.util.Date` і `java.util.Calendar`, які були менш інтуїтивними і більш схильними до помилок. З введенням `java.time.*` в Java 8, стало легше і безпечніше працювати з датами.

Альтернативи `LocalDate` та `Period` можуть включати бібліотеки, як Joda-Time, хоч і у неї зменшилося значення після випуску Java 8. Також можна використовувати `java.util.Calendar` для сумісності зі старим кодом.

Головні моменти — обирайте правильний тип об'єкта для вашої задачі (`LocalDate` для дат, `LocalDateTime` для дати і часу) і переконайтеся, що знаєте про часові зони та переведення часу, якщо працюєте з часовими поясами.

## Дивіться також:

- [Java Date and Time API](https://docs.oracle.com/javase/tutorial/datetime/)
- [Joda-Time](https://www.joda.org/joda-time/)
- [Java 8 Date Time API](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [Робота з часовими поясами в Java](https://www.baeldung.com/java-8-date-time-intro)

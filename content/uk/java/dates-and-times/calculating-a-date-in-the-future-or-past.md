---
title:                "Обчислення дати у майбутньому або минулому"
aliases:
- /uk/java/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:28.348302-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/calculating-a-date-in-the-future-or-past.md"
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

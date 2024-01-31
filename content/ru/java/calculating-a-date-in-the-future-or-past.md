---
title:                "Расчет даты в будущем или прошлом"
date:                  2024-01-28T23:55:51.027602-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"

category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Расчёт даты в будущем или прошлом включает корректировку известной даты на определённое количество дней, месяцев или лет. Программисты делают это для функций, таких как напоминания, сроки истечения и планирование событий.

## Как сделать:

```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculation {
    public static void main(String[] args) {
        LocalDate сегодня = LocalDate.now();
        // Добавить 10 дней к текущей дате
        LocalDate будущаяДата = сегодня.plusDays(10);
        System.out.println("Будущая Дата: " + будущаяДата);

        // Вычесть 2 месяца из текущей даты
        LocalDate прошлаяДата = сегодня.minus(2, ChronoUnit.MONTHS);
        System.out.println("Прошлая Дата: " + прошлаяДата);
    }
}
```

Вывод может выглядеть так:

```
Будущая Дата: 2023-04-30
Прошлая Дата: 2023-02-20
```

## Погружение

До Java 8 манипуляции с датами были трудоёмкими. Старые классы такие как `java.util.Date` и `java.util.Calendar` были подвержены ошибкам и были неудобны в использовании. Пакет `java.time`, введённый в Java 8, исправил это с помощью хорошо продуманных классов, таких как `LocalDate`, `LocalTime` и `ZonedDateTime`.

Альтернативы? В эпоху до Java 8 были популярны сторонние библиотеки, такие как Joda-Time. В наши дни вы всё ещё можете использовать их, но стандартный `java.time` рекомендуется, потому что он официально является частью Java и элегантно обрабатывает переход на летнее время, часовые пояса и високосные годы.

При кодировании расчётов дат учитывайте часовые пояса, если это важно для вашего контекста. Для UTC используйте `Instant` вместо `LocalDate`. Для конкретных зон вы обычно будете использовать `ZonedDateTime`. Помните, операции с датой-временем могут быть объединены, как `date.minusWeeks(1).plusHours(3)`, что делает ваш код чище.

## См. также

1. Обзор пакета `java.time`: [Oracle Docs](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. Обработка временных зон с `ZonedDateTime`: [Oracle ZonedDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
3. Официальные шаблоны даты и времени для `java.time.format.DateTimeFormatter`: [Oracle DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)

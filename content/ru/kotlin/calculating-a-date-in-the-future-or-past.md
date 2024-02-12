---
title:                "Расчет даты в будущем или прошлом"
aliases:
- ru/kotlin/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T23:55:34.434349-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Расчёт будущей или прошедшей даты означает нахождение конкретного дня до или после известной даты. Программисты делают это для функций вроде напоминаний, уведомлений об истечении срока или планирования — всего, что связано с временем.

## Как это сделать:

Kotlin работает с датами и временем с помощью библиотеки `java.time`. Чтобы добавить или вычесть дни, используйте `plusDays()` или `minusDays()`. Вот краткое руководство:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val tenDaysLater = today.plusDays(10)
    val tenDaysBefore = today.minusDays(10)
    
    println("Сегодня: $today")
    println("Через десять дней: $tenDaysLater")
    println("Десять дней назад: $tenDaysBefore")
}
```

Пример вывода:

```
Сегодня: 2023-03-15
Через десять дней: 2023-03-25
Десять дней назад: 2023-03-05
```

Помимо дней, вы также можете играть с месяцами и годами (`plusMonths()`, `minusMonths()`, `plusYears()`, `minusYears()`).

## Подробнее

Расчёт дат не новинка. С Java 8 пакет `java.time` стал основным средством для арифметики даты и времени — гораздо лучше, чем старые `Calendar` или `Date`, которые были громоздкими и не потокобезопасными.

`java.time` использует неизменяемые объекты, так что вы избегаете неприятных ошибок из-за случайного изменения ваших дат. Объекты вроде `LocalDate`, `LocalTime`, `LocalDateTime` и `ZonedDateTime` помогают точно представлять различные аспекты времени.

Альтернативы? Конечно. Перед `java.time` Joda-Time был предпочтительным инструментом. Некоторые старые системы до сих пор используют его. И в сфере Android библиотека ThreeTenABP адаптирует функции `java.time` для совместимости с Java 6 & 7.

API `java.time` также разработано с учётом часовых поясов, благодаря классам вроде `ZonedDateTime`. Так что, когда вы манипулируете датами, вы можете учитывать хронологию вращения Земли.

## Смотрите также

- Официальная документация `java.time`: [Java SE Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- Для разработчиков Android, подробности библиотеки `ThreeTenABP`: [ThreeTenABP на GitHub](https://github.com/JakeWharton/ThreeTenABP)
- Подробное руководство, если вы хотите узнать больше о датах и времени: [Дата и Время в Java](https://www.baeldung.com/java-8-date-time-intro)

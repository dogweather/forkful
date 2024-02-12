---
title:                "Получение текущей даты"
aliases: - /ru/kotlin/getting-the-current-date.md
date:                  2024-01-28T23:58:40.030110-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Мы получаем текущую дату, чтобы знать данные на сегодняшний день. Это крайне важно для множества функций - думайте о логах, пробных периодах, событиях. Назовите это, и даты часто будут прямо там.

## Как:
```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Сегодняшняя дата: $today")
}
```

Пример вывода:
```
Сегодняшняя дата: 2023-04-05
```

## Глубокое погружение
Исторически, даты были кучей проблем для разработчиков. Часовые пояса, високосные годы, переход на летнее время; это сложно. Kotlin опирается на API `java.time` начиная с Java 8, что делает операции с датами более приемлемыми.

`LocalDate.now()` - наш выбор для текущих дат. Без времени, без зоны — только дата. Нужно время? Есть `LocalTime`. И то, и другое? `LocalDateTime`. А если важен часовой пояс, используйте `ZonedDateTime`.

Альтернативы? До Java 8 господствовали `java.util.Date` и `Calendar`. Не отлично, не ужасно, но теперь как-то устарели и, честно говоря, менее интуитивны.

Под капотом `LocalDate.now()` захватывает системные часы. Но это не просто какие-то часы - это часы UTC, скорректированные к часовому поясу вашей системы по умолчанию. Вы можете поменять ее, конечно - передайте другой `Clock` или `ZoneId`, если вам нравится жить на грани.

## Смотрите также
Документация Kotlin о датах и времени: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)

Обзор Java 8 Date/Time: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)

Хотите стать полным историком? Ознакомьтесь с эволюцией java.time: [https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html](https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:40.030110-07:00
description: "\u041A\u0430\u043A: \u0418\u0441\u0442\u043E\u0440\u0438\u0447\u0435\
  \u0441\u043A\u0438, \u0434\u0430\u0442\u044B \u0431\u044B\u043B\u0438 \u043A\u0443\
  \u0447\u0435\u0439 \u043F\u0440\u043E\u0431\u043B\u0435\u043C \u0434\u043B\u044F\
  \ \u0440\u0430\u0437\u0440\u0430\u0431\u043E\u0442\u0447\u0438\u043A\u043E\u0432\
  . \u0427\u0430\u0441\u043E\u0432\u044B\u0435 \u043F\u043E\u044F\u0441\u0430, \u0432\
  \u0438\u0441\u043E\u043A\u043E\u0441\u043D\u044B\u0435 \u0433\u043E\u0434\u044B\
  , \u043F\u0435\u0440\u0435\u0445\u043E\u0434 \u043D\u0430 \u043B\u0435\u0442\u043D\
  \u0435\u0435 \u0432\u0440\u0435\u043C\u044F; \u044D\u0442\u043E \u0441\u043B\u043E\
  \u0436\u043D\u043E. Kotlin \u043E\u043F\u0438\u0440\u0430\u0435\u0442\u0441\u044F\
  \ \u043D\u0430 API\u2026"
lastmod: '2024-04-05T21:53:45.510785-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0441\u0442\u043E\u0440\u0438\u0447\u0435\u0441\u043A\u0438, \u0434\
  \u0430\u0442\u044B \u0431\u044B\u043B\u0438 \u043A\u0443\u0447\u0435\u0439 \u043F\
  \u0440\u043E\u0431\u043B\u0435\u043C \u0434\u043B\u044F \u0440\u0430\u0437\u0440\
  \u0430\u0431\u043E\u0442\u0447\u0438\u043A\u043E\u0432."
title: "\u041F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0443\
  \u0449\u0435\u0439 \u0434\u0430\u0442\u044B"
weight: 29
---

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

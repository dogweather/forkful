---
date: 2024-01-20 17:37:32.067294-07:00
description: "How to (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438): ."
lastmod: '2024-03-13T22:44:49.238696-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## How to (Як це зробити):
```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDateTime = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")
    val formattedDate = currentDateTime.format(formatter)
    println(formattedDate)
}
```
Sample output (Приклад виведення):
```
24-03-2023 15:45:10
```

## Deep Dive (Поглиблений Розбір):
Исторично, форматування дати як строки користувалось різними бібліотеками в Java, такими як `SimpleDateFormat`. З різницями в часових зонах і локалізаціями, уникнути плутанини було важко. Kotlin, використовуючи Java Time API (введений у Java 8), призводить до покращеного досвіду з `DateTimeFormatter`.

Альтернативи включають сторонні бібліотеки, такі як Joda-Time, яку колись широко використовували до Java 8, і прийнятні системні методи, такі як `toString()`, які можуть не відповідати всім потребам.

Ключ до поняття – формат. `DateTimeFormatter` дає можливість вказати точний формат (наприклад `dd-MM-yyyy HH:mm:ss` для день-місяць-рік та години:хвилини:секунди) і враховує локалізацію, що критично для багатомовних застосунків.

## See Also (Дивіться також):
- [Java 8 DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Kotlin API reference](https://kotlinlang.org/api/latest/jvm/stdlib/)
- [Joda-Time library](https://www.joda.org/joda-time/)

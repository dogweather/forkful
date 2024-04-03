---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:03.172609-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Kotlin \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u0440\u043E\u0437\
  \u0431\u0456\u0440 \u0434\u0430\u0442 \u0447\u0435\u0440\u0435\u0437 \u043F\u0430\
  \u043A\u0435\u0442 `java.time`, \u0449\u043E \u0431\u0443\u0432 \u0432\u0432\u0435\
  \u0434\u0435\u043D\u0438\u0439 \u0443 Java 8. \u041E\u0441\u044C \u043F\u0440\u043E\
  \u0441\u0442\u0438\u0439 \u043F\u0456\u0434\u0445\u0456\u0434 \u0437\u0430 \u0434\
  \u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E `LocalDateTime` \u0442\u0430\u2026"
lastmod: '2024-03-13T22:44:49.235206-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u0440\u043E\
  \u0437\u0431\u0456\u0440 \u0434\u0430\u0442 \u0447\u0435\u0440\u0435\u0437 \u043F\
  \u0430\u043A\u0435\u0442 `java.time`, \u0449\u043E \u0431\u0443\u0432 \u0432\u0432\
  \u0435\u0434\u0435\u043D\u0438\u0439 \u0443 Java 8."
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
weight: 30
---

## Як це зробити:
Kotlin підтримує розбір дат через пакет `java.time`, що був введений у Java 8. Ось простий підхід за допомогою `LocalDateTime` та специфічного шаблону:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // Виведення: 2023-04-01T12:00
}
```

Для більшої гнучкості чи для обробки дат з зовнішніх джерел, таких як API, можна скористатися сторонньою бібліотекою, такою як Joda-Time (хоча зараз це рідше використовується через робастність `java.time`). Проте, для більшості додатків на Kotlin переважно використовувати сучасний підхід, що надається JDK.

Для розбору дати в Kotlin без використання сторонніх бібліотек можна також скористатися класом `SimpleDateFormat` для версій до Java 8 або рівнів API Android, які не підтримують `java.time`:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // Виведення може відрізнятися залежно від вашого часового поясу, наприклад, Суб кві 01 12:00:00 GMT 2023
}
```

Пам'ятайте завжди встановлювати часовий пояс при роботі з `SimpleDateFormat` для уникнення несподіваних зміщень у розібраних датах.

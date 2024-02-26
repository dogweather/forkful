---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:03.172609-07:00
description: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437\
  \ \u0440\u044F\u0434\u043A\u0430 \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443\
  \ \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u0456 \u0442\
  \u0435\u043A\u0441\u0442\u0443 \u043D\u0430 \u043E\u0431'\u0454\u043A\u0442 Date.\
  \ \u0426\u044F \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u044F \u0454 \u0444\u0443\
  \u043D\u0434\u0430\u043C\u0435\u043D\u0442\u0430\u043B\u044C\u043D\u043E\u044E \u0434\
  \u043B\u044F \u0434\u043E\u0434\u0430\u0442\u043A\u0456\u0432, \u044F\u043A\u0456\
  \ \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u044E\u0442\u044C \u0437 \u0434\
  \u0430\u0442\u0430\u043C\u0438, \u0432\u0432\u0435\u0434\u0435\u043D\u0438\u043C\
  \u0438\u2026"
lastmod: '2024-02-25T18:49:46.716428-07:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430 \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u043F\
  \u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u0456 \u0442\u0435\u043A\
  \u0441\u0442\u0443 \u043D\u0430 \u043E\u0431'\u0454\u043A\u0442 Date. \u0426\u044F\
  \ \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u044F \u0454 \u0444\u0443\u043D\u0434\
  \u0430\u043C\u0435\u043D\u0442\u0430\u043B\u044C\u043D\u043E\u044E \u0434\u043B\u044F\
  \ \u0434\u043E\u0434\u0430\u0442\u043A\u0456\u0432, \u044F\u043A\u0456 \u0432\u0437\
  \u0430\u0454\u043C\u043E\u0434\u0456\u044E\u0442\u044C \u0437 \u0434\u0430\u0442\
  \u0430\u043C\u0438, \u0432\u0432\u0435\u0434\u0435\u043D\u0438\u043C\u0438\u2026"
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## Що і чому?
Розбір дати з рядка полягає у перетворенні тексту на об'єкт Date. Ця операція є фундаментальною для додатків, які взаємодіють з датами, введеними користувачами або отриманими з зовнішніх наборів даних, дозволяючи легко маніпулювати ними та форматувати відповідно до потреб.

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

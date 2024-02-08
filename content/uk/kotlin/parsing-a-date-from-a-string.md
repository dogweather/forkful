---
title:                "Розбір дати з рядка"
aliases:
- uk/kotlin/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:03.172609-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

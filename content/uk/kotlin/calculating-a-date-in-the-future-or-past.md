---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:31:45.050721-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому?
Розрахунок дати в майбутньому чи минулому — це просто зміщення відліку часу на певний період. Програмісти це роблять для обробки подій, резервування, моніторингу чи прогнозування.

## Як робити:
В Kotlin ми використовуємо клас `LocalDate` з пакету `java.time`. Ось як це працює:

```Kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val today = LocalDate.now()
    val tenDaysLater = today.plusDays(10)
    val twoWeeksEarlier = today.minusWeeks(2)
  
    println("Сьогодні: $today")
    println("Через 10 днів: $tenDaysLater")
    println("Дві неділі тому: $twoWeeksEarlier")
}
```

Вам покажуть поточну дату, дату через 10 днів, і дату дві неділі назад.

## Поглиблено:
Раніше для роботи з датами програмісти використовували `java.util.Date` та `java.util.Calendar`. Однак, з Java 8 з'явився новий API для дати і часу - `java.time`, котрий і взяла за основу Kotlin. 

Ви також можете використовувати `plus` і `minus` з різними одиницями часу, як-от `ChronoUnit.DAYS` для днів чи `ChronoUnit.WEEKS` для тижнів. Це надає гнучкості роботі з часовими періодами.

Окрім того, кутлінівська бібліотека KotlinX.datetime додає додаткові функції для роботи з датою і часом, особливо корисні для cross-platform розробки.

## Також подивіться:
- [Документація по API java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [KotlinX.datetime бібліотека](https://github.com/Kotlin/kotlinx-datetime)
- [Путівник по міграції з java.util.Date на java.time](https://www.baeldung.com/migrating-to-java-8-date-time-api)
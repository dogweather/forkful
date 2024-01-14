---
title:                "Kotlin: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому
Поточна дата є важливою частиною багатьох програм, незалежно від того, чи це веб-додаток, мобільний додаток або десктопна програма. Отримання поточної дати дозволяє нам відстежувати час, зберігати дані та адаптувати програму до різних часових зон.

## Як виконати
Існує декілька способів отримати поточну дату в Kotlin, але ми розглянемо найпростіший із них - використання класу `LocalDate`. Для цього нам необхідно імпортувати пакет `java.time` та створити об'єкт `LocalDate` з використанням функції `now()`:
```Kotlin
import java.time.LocalDate

val currentDate = LocalDate.now()
```
Після цього у змінній `currentDate` буде зберігатися поточна дата.

Також, ми можемо отримати дату з конкретною часовою зоною, використовуючи об'єкт `ZoneId`. Наприклад, якщо ми хочемо отримати дату з часовою зоною "America/New_York", ми можемо використати такий код:
```Kotlin
val currentDate = LocalDate.now(ZoneId.of("America/New_York"))
```

## Поглиблене вивчення
Клас `LocalDate` містить багато корисних методів для роботи з датами. Наприклад, ми можемо отримати день тижня з допомогою функції `getDayOfWeek()` або перевірити, чи є дата високосною з допомогою функції `isLeapYear()`.

Крім того, клас `LocalDate` є одним із класів, що реалізують інтерфейс `Temporal`, який дозволяє нам виконувати різні операції над датами, такі як додавання днів, місяців або років до поточної дати.

## Дивись також
- [Документація по класу LocalDate в Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/)
- [Офіційний сайт Kotlin](https://kotlinlang.org/)
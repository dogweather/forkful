---
title:                "Обчислення дати в майбутньому або минулому"
html_title:           "Kotlin: Обчислення дати в майбутньому або минулому"
simple_title:         "Обчислення дати в майбутньому або минулому"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому
Ви могли б замислитися про обчислення дати у майбутньому або минулому, якщо вам потрібно розрахувати дату події чи планувати подорож в певну дату.

## Як це зробити
Використовуйте функцію `LocalDate.plus()` у Kotlin, щоб додати або відняти деяку кількість днів, тижнів, місяців або років до дати. Наприклад, щоб отримати дату через 2 тижні від поточної дати, використовуйте `LocalDate.now().plusWeeks(2)`. Результат буде видавати об'єкт `LocalDate` з новою датою. Нижче наведено приклад коду і його вихідний результат, який можна перевірити у вашому коді.

```Kotlin
val currentDate = LocalDate.now()
println(currentDate.plusWeeks(2)) // Виведе дату через 2 тижні від поточної
```
Вихідний результат: `2021-04-13`

```Kotlin
val futureDate = LocalDate.of(2022, Month.JANUARY, 1)
println(futureDate.minusMonths(3)) // Виведе дату, змінену на 3 місяці назад
```
Вихідний результат: `2021-10-01`

## Поглиблене дослідження
Аби отримати більше інформації про роботу з датами у Kotlin, ви можете переглянути офіційну документацію на сайті [Kotlin](https://kotlinlang.org/docs/dates.html). Також можете дізнатися більше про тип `LocalDate` та його методи у [документації Java](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/time/LocalDate.html).

## Див. також
- [Офіційна документація Kotlin](https://kotlinlang.org/docs/home.html)
- [Робота з датами в Java](https://www.baeldung.com/java-8-date-time-intro)
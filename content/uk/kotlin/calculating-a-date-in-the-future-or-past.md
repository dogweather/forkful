---
title:                "Kotlin: Обчислення дати у майбутньому або минулому"
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Розрахунок дати в майбутньому або минулому є необхідним для багатьох програм, особливо в областях, пов'язаних зі знімною або обробкою даних. Також, це може бути корисним для створення календарів або планування подій. Таким чином, вміння обчислювати дати у майбутньому та минулому є важливим для розробників програм на мові Kotlin.

## Як це зробити

Розрахунок дати у майбутньому й майбутньому в Kotlin досить простий і виконується за допомогою вбудованого класу `Calendar` та його методу `add`.

```
Kotlin
val date = Calendar.getInstance() //отримуємо поточну дату
date.add(Calendar.DAY_OF_MONTH, 7) //додаємо 7 днів до поточної дати
println(date.time) //виводимо результуючу дату
```

Це дозволить нам розрахувати дату, віддалену від поточної на 7 днів. Аналогічно можна додавати і віднімати інші одиниці часу, такі як місяці або роки.

```Kotlin
val date = Calendar.getInstance()
date.add(Calendar.MONTH, -2) //віднімаємо 2 місяці від поточної дати
println(date.time)
```

## Глибоке дослідження

Для більш складних розрахунків, таких як визначення дати наступного вівторка або останньої п'ятниці місяця, можна використовувати бібліотеку `java.time` з Java 8.

```Kotlin
val date = LocalDate.now() //отримуємо поточну дату
val nextTuesday = date.with(TemporalAdjusters.next(DayOfWeek.TUESDAY)) //визначаємо наступний вівторок
println(nextTuesday) //виводимо результат
```

Також, варто пам'ятати про різницю між часовими зонами при розрахунках дат у різних країнах або регіонах. Для цього можна використовувати клас `ZoneId` з бібліотеки `java.time`.

## Дивись також

- [Документація по класу Calendar у Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-calendar/)
- [Офіційна документація по бібліотеці java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Навчальне відео по обчисленню дат у Kotlin](https://www.youtube.com/watch?v=S1mCwvF_UHo)
---
title:    "Kotlin: Отримання поточної дати"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Написання програм - це цікаве та захоплююче заняття, але часто виникає потреба в отриманні поточної дати. Це може бути необхідно для ведення логів, створення звітів або просто для відстеження часу. У цій статті ми розглянемо як саме отримати поточну дату в Kotlin та покладатися на це завдання нашому коду.

## Як

Існує кілька способів отримати поточну дату в Kotlin. Один із них - використовувати стандартну бібліотеку з датами. Для цього нам потрібно імпортувати клас `java.time.LocalDate` та використати метод `now()`:

```Kotlin
import java.time.LocalDate

val currentDate = LocalDate.now()
println(currentDate)
```

В результаті ми отримаємо поточну дату у форматі `yyyy-MM-dd`. Якщо нам потрібно отримати дату у більш зручному для нас форматі, наприклад, `dd/MM/yyyy`, ми можемо використати метод `format()` та вказати необхідний нам шаблон:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val currentDate = LocalDate.now()
val formattedDate = currentDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
println(formattedDate)
```

Це всього лише один з прикладів, як можна отримати поточну дату в Kotlin. Інші можливості включають використання бібліотеки `java.util.Date` або вбудованого в Kotlin `LocalDateTime`.

## Deep Dive

Одним з способів отримання поточної дати в Kotlin є використання `System.currentTimeMillis()`. Цей метод повертає кількість мілісекунд, що пройшли з 1 січня 1970 року, і називається EPOCH. Отже, якщо нам потрібно отримати поточну дату у мілісекундах, ми можемо скористатися таким кодом:

```Kotlin
val currentTimeInMillis = System.currentTimeMillis()
println(currentTimeInMillis)
```

Також існує можливість використовувати функцію `Instant.now()` для отримання поточної дати у вигляді об'єкта `Instant`, а потім конвертувати його до потрібного нам формату за допомогою `DateTimeFormatter`.

У загальному, отримання поточної дати в Kotlin - це нескладне завдання і залежить від вашої конкретної потреби та вимог.

## Дивіться також

1. [Офіційна документація Kotlin](https://kotlinlang.org/docs/reference/datetime.html)
2. [Робота з датою та часом в Kotlin](https://www.geeksforgeeks.org/kotlin-date-time/)
3. [Стандартні функції для роботи з датами в Kotlin](https://proandroiddev.com/date-and-time-in-java-android-and-kotlin-f78063b64671)
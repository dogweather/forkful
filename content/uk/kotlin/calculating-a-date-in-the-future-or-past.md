---
title:    "Kotlin: Обчислення дати в майбутньому або минулому."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому: 
Обчислення дати у майбутньому або минулому може бути корисним при різних програмних завданнях, таких як планування подій або розрахунків бюджету. 

## Як: 
```Kotlin

// Імпорт необхідних бібліотек
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    // Обчислюємо дату п'ять днів в майбутньому
    val futureDate = LocalDate.now().plusDays(5)

    // Форматуємо дату за допомогою шаблону "dd.MM.yyyy"
    val formattedFutureDate = futureDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))

    // Виводимо результат
    println("Дата через 5 днів: $formattedFutureDate") // Виводиться "Дата через 5 днів: 30.05.2021"

    // Обчислюємо дату два роки назад
    val pastDate = LocalDate.now().minusYears(2)

    // Форматуємо дату за допомогою шаблону "dd.MM.yyyy"
    val formattedPastDate = pastDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))

    // Виводимо результат
    println("Дата два роки тому: $formattedPastDate") // Виводиться "Дата два роки тому: 27.05.2019"
}

```

## Profound Dive: 
За допомогою стандартного класу `LocalDate` та його методів `plusDays()` та `minusYears()` ми можемо легко обчислювати дати у майбутньому та минулому. Також, за допомогою методу `format()` та класу `DateTimeFormatter` ми можемо форматувати дату у зручному для нас вигляді. 

## Дивіться також: 
- [Документація про клас `LocalDate`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/)
- [Стаття про форматування дати в Kotlin](https://kotlinexpertise.com/kotlin-date-time-api/)
- [Приклади використання вбудованих методів для обчислення дати у майбутньому та минулому](https://www.geeksforgeeks.org/kotlin-localdate-minusyears-minusmonths-minusweeks-minusdays-must-see/)

Блог-пост написаний на мові програмування Kotlin, яку можна використовувати для широкого спектру завдань, включаючи обчислення дат у майбутньому та минулому. Надіємося, що ця стаття була корисною для вас, і ви будете використовувати ці знання у вашій роботі з Kotlin!
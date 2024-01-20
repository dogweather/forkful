---
title:                "Обчислення дати у майбутньому або минулому"
html_title:           "Kotlin: Обчислення дати у майбутньому або минулому"
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Розрахунок дати в майбутньому або минулому - це процес визначення дати, зміщеної від початкової точки на певну кількість часових періодів. Програмісти використовують це для обробки даних, що змінюються з часом, таких як терміни дії договорів, здійснення платежів, планування задач, тощо.

## Як це робиться:

Я використаю бібліотеку java.time для цього прикладу. За допомогою неї можна отримати дату, зміщену від початкової дати на певний часовий період.

```Kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val currentDate = LocalDate.now()
    println("Сьогодні: $currentDate")

    val futureDate = currentDate.plus(5, ChronoUnit.DAYS)
    println("Прибуде майбутнього: $futureDate")

    val pastDate = currentDate.minus(10, ChronoUnit.MONTHS)
    println("Було в минулому: $pastDate")
}
```

Цей код виведе:

```Kotlin
Сьогодні: 2022-05-10
Прибуде майбутнього: 2022-05-15
Було в минулому: 2021-07-10
```

## Занурення в деталі:

Розрахунок майбутніх та минулих дат є важливою частиною практично кожного софту, що використовує часові даних. Однак, зважаючи на всі проблеми, пов'язані з часовими поясами, переходами на літній/зимовий час та календарем, це може бути дуже складним.

Альтернативи розрахунку зміщених дат включають використання "Calendar" або "Date" в старих версіях Java, але вони не рекомендуються через їхні відомі проблеми.

Kotlin не має вбудованого способу робити це, тому ми використовуємо java.time, що було додано в Java 8, щоб зробити роботу з датами та часом більш приємною.

## Додаткова інформація:

1. [Kotlin Docs](https://kotlinlang.org/docs/home.html)
2. [Java.Time](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
3. [Old Date and Time API vs. Java 8 Date and Time API](https://www.javatpoint.com/java-8-date-and-time-api)
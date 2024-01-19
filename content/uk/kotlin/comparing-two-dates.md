---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

(Українська версія)

# Порівняння Дат в Kotlin

## Що і Чому?

Порівняння двох дат - це спосіб визначення порядку подій в часі в програмах. Це важливо для створення функціоналу, пов'язаного з календарем, розрахунком термінів тощо.

## Як це зробити:

В Kotlin для порівняння двох дат ви можете використовувати методи `isAfter`, `isBefore` та `isEqual`.

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2022, 1, 1)
    val date2 = LocalDate.of(2022, 12, 31)

    println(date1.isBefore(date2))  // Виведе: true
    println(date1.isAfter(date2))   // Виведе: false
    println(date1.isEqual(date2))   // Виведе: false
}
```

## Глибше в тему:

**Історичний контекст:** Структури даних для дат були вперше введені в мовах програмування в 1960-х роках. Однак, тоді методи порівняння були дуже примітивні. Kotlin використовує LocalDate з Java для праці з датами, що значно спрощує життя розробникам.

**Альтернативи:** Є й інші бібліотеки для роботи з датами, наприклад, Joda-Time. Kotlin також може використовувати `compareTo` для порівняння дат.

**Деталі реалізації:** Метод `isBefore` порівнює дати, перетворюючи їх у мілісекунди від початкової епохи (1 січня 1970 року). Коли ці числа порівнюються, ми отримуємо результат.

## Див. також:

1. [Документація Kotlin по `LocalDate`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/index.html)
2. [Порівняння дат в Kotlin на StackOverflow](https://stackoverflow.com/questions/56706749/how-to-compare-dates-in-kotlin)
3. [Документація по Joda-Time](https://www.joda.org/joda-time/)
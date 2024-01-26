---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:37:17.096459-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Парсинг дати із рядка — це процес перетворення текстової інформації про дату і час у структурований формат, з яким можна легко працювати в коді. Програмісти роблять це, аби керувати датами, здійснювати розрахунки, фільтрацію та сортування даних.

## Як це зробити:
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-01"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val parsedDate = LocalDate.parse(dateString, formatter)

    println(parsedDate) // Виводить: 2023-04-01
}
```
Вищенаведений код перетворює рядок з датою в об'єкт LocalDate, використовуючи формат `yyyy-MM-dd`.

## Поглиблений розгляд
Перши методи парсингу дат в Java з'явилися давно і були досить громіздкими та нестійкими через клас `SimpleDateFormat`. У Kotlin рекомендують користуватися `DateTimeFormatter` з Java Time API, що з'явився у Java 8. Це дає ширші можливості та більшу надійність. Як альтернативу `DateTimeFormatter`, можна використовувати бібліотеки третіх сторін, такі як Joda-Time, але з появою стандартизованого Java Time API потреба в них зменшилася. Важливо обрати вірний формат дати, щоб уникнути `DateTimeParseException` при спробі парсинга невідповідного рядка.

## Додаткові ресурси
- Документація Kotlin: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- Офіційна документація Java `DateTimeFormatter`: [https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Про Java Time API: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- Бібліотека Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)

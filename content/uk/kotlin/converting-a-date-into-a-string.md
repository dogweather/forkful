---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення дати в рядок - це процес, коли дата змінює свій формат з типу даних "Дата" на рядок. Програмісти роблять це, щоб візуалізувати дату в зручному, зрозумілому людям форматі.

## Як це зробити:
Ось базовий приклад перетворення дати в рядок в Kotlin:
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main(args: Array<String>) {
    val currentdate = LocalDate.now()
    val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val formatted = currentdate.format(formatter)
    println("Дата в форматі рядка: $formatted")
}
```
Цей код переведе поточну дату у прийнятний формат і виведе його. Наприклад, якщо сьогодні 22 шілня 2022 року, ви побачите: "Дата в форматі рядка: 22-11-2022".

## Поглиблений занурення

Перетворення дати в рядок - давня практика, сформована з необхідності записати дату в текстові документи та інтерфейси. Є різні способи це зробити, але в Kotlin найбільш поширеним є `DateTimeFormatter`. Щодо впровадження, `DateTimeFormatter` бере шаблон і використовує його для форматування дати в рядок, виконуючи реальну зміну типів даних в рантаймі.

## Дивіться ще

[Введення в DateTime API в Kotlin](https://kotlinlang.org/docs/dates-times.html)

[Процес форматування дати в Kotlin](https://www.baeldung.com/kotlin/dates) 

[Tutorial: Patterns for Formatting and Parsing](http://tutorials.jenkov.com/java-internationalization/simpledateformat.html)